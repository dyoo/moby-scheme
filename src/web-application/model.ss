#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/path
         scheme/file
         scheme/list
         "../compile-world.ss"
         "../utils.ss"
         (planet jaymccarthy/sqlite:3))

(require (for-syntax scheme/base))


;; The model we maintain has a maintenance thread, a request channel, and a response channel.
;; data-dir is a path that we use to store things like the database and other disk-based
;; resources.
(define-struct model (th       ;; maintenance thread
                      req-ch   ;; request channel
                      res-ch   ;; response channel
                      data-dir ;; absolute path
                      db
                      ) #:mutable)


(define-struct user (id ;; number 
                     name ;; string
                     email ;; string
                     moderated? ;; boolean
                     ))



(define-struct source (id             ;; string
                       program-name   ;; string
                       code           ;; bytes
                       date-submitted ;; date
                       user           ;; user
                       ))


(define-struct binary (id 
                       name      ;; string
                       package   ;; bytes
                       approved? ;; boolean
                       source    ;; source
                       ))

;;;;



(define-struct platform ())
(define-struct (platform:j2me platform) ())
(define-struct (platform:android platform) ())

;;;;;;


;; model-scratch-dir: model -> path
;; Returns the scratch directory used for compilation.
(define (model-scratch-directory a-model)
  (let ([p (build-path (model-data-dir a-model) "scratch")])
    (make-directory* p)
    p))
  

;; model-db-path: model -> path
;; Returns the path of the database.
(define (model-db-path a-model)
  (build-path (model-data-dir a-model) "db.sqlite"))



;; make-model: path-string -> model
;; Creates a new model.
(define (-make-model data-path)  
  (let* ([data-path (normalize-path data-path)]
         [a-model (make-model #f
                              (make-channel) 
                              (make-channel) 
                              data-path
                              #f)])
    (make-directory* data-path)
    (set-model-db! a-model (open (model-db-path a-model)))
    (install-tables! a-model)
    (set-model-th! a-model 
                   (thread (lambda () 
                             (request-loop a-model))))
    a-model))



(define create-statements
  (regexp-split #rx"\n\n"
  #<<EOF
create table if not exists
             user (id INTEGER primary key,
                   name TEXT not null,
                   email TEXT not null unique,
                   is_moderated INTEGER not null default 0);

create table if not exists
             source (id INTEGER primary key,
                     name TEXT not null,
                     code BLOB not null,
                     date_submitted TEXT not null,
                     user_id INTEGER not null);

create table if not exists
             binary (id INTEGER primary key,
                     name TEXT not null,
                     package BLOB not null,
                     is_visible INTEGER not null default 1,
                     downloads INTEGER not null default 0,
                     source_id INTEGER not null)
EOF
))
;; install-tables!: model -> void
;; Installs the necessary tables if they don't already exist.
(define (install-tables! a-model)
  (for ([stmt create-statements])
    (exec/ignore (model-db a-model) stmt)))



;; Request types:
(define-struct req:thunk (thunk))
(define-struct req:quit ())


;; request-loop: model -> void
;; Does not return; it's running under the model's maintenance thread.
;; Request-handling loop
(define (request-loop a-model)
  (let ([req-ch (model-req-ch a-model)]
        [res-ch (model-res-ch a-model)])
    (let loop ()
      (let* ([req (channel-get req-ch)]
             [res (with-handlers ([exn:fail? 
                                   ;; Throw exceptions back up as values.
                                   values])
                    (match req 
                      [(struct req:thunk (thunk))
                       (let ([result
                              (with-handlers ([void (lambda (exn)
                                                      exn)])
                                (thunk))])
                         (channel-put res-ch result)
                         (loop))]
                      [(struct req:quit ()) 
                       (void)]))])
        (channel-put res-ch res)))))



;; send-request: model request -> response
;; Sends a request off to the maintenance thread, and waits for a response.
;; If we got an exception, throws it back to the user.
(define (send-request a-model a-req)
  (thread-resume (model-th a-model) (current-thread))
  (channel-put (model-req-ch a-model) a-req)
  (let ([response (channel-get (model-res-ch a-model))])
    (when (exn? response)
      (raise response))
    response))



;; with-serializing: model thunk -> X
(define (with-serializing a-model a-thunk)
  (send-request a-model (make-req:thunk a-thunk)))



;; model-compile-source: model source platform -> binary              
(define (model-compile-source a-model a-source a-platform)
  (define (do-platform-compilation generate binary-find)
    (let* ([name (source-program-name a-source)]
           [dir (make-temporary-directory #:parent-directory (model-scratch-directory a-model))]
           [program-path (build-path dir (string-append name ".ss"))])
      (call-with-output-file program-path 
        (lambda (op)
          (write-bytes (source-code a-source) op)))
      (generate name program-path dir)
      
      (let* ([bin-path (first (find-files binary-find (build-path dir "bin")))]
             [bin (make-binary 
                   #f 
                   (path->string (file-name-from-path bin-path))
                   (get-file-bytes bin-path)
                   (not (user-moderated? (source-user a-source))) ;; approved?
                   a-source                                       ;; source
                   )])
        #;(delete-directory/files dir)
        bin)))
  (with-serializing 
   a-model
   (lambda ()
     (match a-platform
       [(struct platform:j2me ())
        (do-platform-compilation generate-j2me-application jar-path?)]
       [(struct platform:android ())
        (do-platform-compilation generate-android-application apk-path?)]))))




;; apk-path?: path -> boolean
;; Returns true if the path looks like an Android package.
(define (apk-path? a-path)
  (and (filename-extension a-path)
       (bytes=? #"apk" (filename-extension a-path))))


;; jar-path?: path -> boolean
;; Returns true if the path looks like a jar package.
(define (jar-path? a-path)
  (and (filename-extension a-path)
       (bytes=? #"jar" (filename-extension a-path))))



;; close-model: model -> void
;; Turns off the model.
(define (close-model a-model)
  (send-request a-model (make-req:quit))
  (close (model-db a-model)))




;; delete-model: model -> void
;; Destroys the model.
(define (delete-model! a-model)
  (close-model a-model)
  (delete-directory/files (model-data-dir a-model)))


(define in-transaction? (make-parameter #f))

;; Syntax do handle both transactions and prepared statements.
;; Also handles nested in-transaction.
(define-syntax (with-transaction/stmts stx)
  (syntax-case stx ()
    [(_ (db abort (prep-id prep-stmt-string) ...)
        body ...)
     (syntax/loc stx
       (let ([thunk (lambda () 
                      (let ([prep-id (prepare db prep-stmt-string)] ...)
                        (dynamic-wind
                         (lambda () (void))
                         (lambda () body ...)
                         (lambda () (finalize prep-id) ...))))])
         (cond [(in-transaction?)
                (thunk)]
               [else
                (parameterize ([in-transaction? #t])
                  (with-transaction 
                   (db abort)
                   (thunk)))])))]))




;; model-add-user!: model string string -> void
;; Adds a user to the model.  If the user already exists,
;; raises an error.
;; FIXME: do the error trapping.

(define (model-add-user! a-model name email)
  (with-transaction/stmts
   ((model-db a-model) 
    abort 
    [add-user-stmt "insert into user (name, email) values (?, ?)"])
   (run add-user-stmt name email)
   (model-find-user a-model email)))


;; model-find-user: model string -> (or/c user #f)
;; Looks up a user in the model.
(define (model-find-user a-model an-email)
  (with-transaction/stmts
   ((model-db a-model) 
    abort
    [find-user-stmt 
     "select id, name, email, is_moderated from user where email=?"])
   (load-params find-user-stmt an-email)
   (cond
     [(step find-user-stmt)
      =>
      (lambda (v) (apply make-user (vector->list v)))]
     [else
      #f])))








(provide/contract [rename -make-model make-model (path-string? . -> . model?)]
                  [close-model (model? . -> . any)]
                  [delete-model! (model? . -> . any)]                  
                  
                  [model-add-user! (model? string? string? . -> . user?)]
                  [model-find-user (model? string? . -> . (or/c user? false/c))]
                  
                  [model-compile-source (model? source? platform? . -> . binary?)]

                  
                  [struct user ([id number?]
                                [name string?]
                                [email string?]
                                [moderated? boolean?])]
                  
                  [struct source ([id number?]
                                  [program-name string?]
                                  [code bytes?]
                                  [date-submitted date?]
                                  [user user?])]
                  
                  [struct binary ([id any/c #;number?]
                                  [name string?]
                                  [package bytes?]
                                  [approved? boolean?]
                                  [source source?])]
                  
                  [struct platform ()]
                  [struct (platform:j2me platform) ()]
                  [struct (platform:android platform) ()])
