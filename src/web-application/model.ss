#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/path
         scheme/file
         scheme/class
         (planet untyped/snooze:2:1/snooze)
         (planet untyped/snooze:2:1/sqlite3/sqlite3))

(require (for-syntax scheme/base))


;; The model we maintain has a maintenance thread, a request channel, and a response channel.
;; data-dir is a path that we use to store things like the database and other disk-based
;; resources.
(define-struct model (th       ;; maintenance thread
                      req-ch   ;; request channel
                      res-ch   ;; response channel
                      data-dir ;; absolute path
                      snooze
                      ) #:mutable)



;; email assumed to be unique.
(define-persistent-struct user ([name type:string] 
                                [email type:string]))
(define-persistent-struct program (;; name used to identify the program
                                   [name type:string]
                                   ;; source-code-path should be relative to
                                   ;; data-dir
                                   [source-code-path type:string]
                                   [jar-path type:string]
                                   [jad-path type:string]
                                   [date type:time-utc]
                                   ;; References user
                                   [owner-id type:integer]))

;; Snooze helpers
(define current-snooze (make-parameter #f))

(define (call-with-connection thunk)
  (send (current-snooze) call-with-connection thunk))

(define (save! thing)
  (send (current-snooze) save! thing))

(define (find-one query)
  (send (current-snooze) find-one query))

(define (find-all query)
  (send (current-snooze) find-all query))

(define (table-exists? a-table)
  (send (current-snooze) table-exists? a-table))

(define (create-table a-table)
  (send (current-snooze) create-table a-table))
;;;;



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
    (set-model-snooze! a-model 
                       (make-snooze 
                        (make-database 
                         (build-path data-path "model.sqlite"))))
    (parameterize ([current-snooze (model-snooze a-model)])
      (install-tables!)
      (set-model-th! a-model 
                     (thread (lambda () 
                               (request-loop a-model)))))
    a-model))


(define (install-tables!)
  (call-with-connection
   (lambda ()
     (unless (table-exists? entity:user)
       (create-table entity:user))
     (unless (table-exists? entity:program)
       (create-table entity:program)))))



;; Request types:
(define-struct req:add-user! (name email))
(define-struct req:find-users ())
(define-struct req:find-user/email (email))
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
                      [(struct req:add-user! (name email))
                       (-add-user! a-model name email)
                       (loop)]
                      [(struct req:find-users ())
                       (-find-users a-model)
                       (loop)]
                      [(struct req:find-user/email (email))
                       (-find-user/email a-model email)
                       (loop)]
                      [(struct req:quit ()) 
                       (void)]))])
        (channel-put res-ch res)))))



;; send-request: model request -> response
;; Sends a request off to the maintenance thread, and waits for a response.
;; If we got an exception, throws it back to the user.
(define (send-request a-model a-req)
  (thread-resume (model-th a-model) (current-thread))
  (channel-put (model-req-ch a-model) a-req)
  (let ([res (channel-get (model-res-ch a-model))])
    (when (exn:fail? res)
      (raise res))
    res))


;; add-user! model string string -> user
;; Adds a user to the system.
(define (add-user! a-model name email)
  (when (find-user/email a-model email)
    (error 'add-user "User with email address ~s already exists" email))
  (send-request a-model (make-req:add-user! name email)))
;; Internal.
(define (-add-user! a-model name email)
  (call-with-connection 
   (lambda () 
     (save! (make-user name email)))))



;; find-users: model -> (listof user)
;; Finds all of the users.
(define (find-users a-model)
  (send-request a-model (make-req:find-users)))
;; Internal.
(define (-find-users a-model)
  (call-with-connection 
   (lambda () 
     (find-all 
      (let-alias ([U user]) (sql:select #:from U))))))


;; find-user/email: model string -> (or/c user #f)
;; Looks for a user by unique email.
(define (find-user/email a-model email)
  (send-request a-model (make-req:find-user/email email)))
(define (-find-user/email a-model email)
  (call-with-connection
   (lambda ()
     (find-one
      (let-alias ([P user])
         (sql:select #:from P
                     #:where (sql:= P-email email)))))))



;; add-program!: user string bytes -> program
;; Adds a program.
#;(define (add-program! user name source-code)
  ...)



#;(define (get-package-jar program)
  ...)


#;(define (get-package-jad program)
  ...)


;; close-model: model -> void
;; Turns off the model.
(define (close-model a-model)
  (send-request a-model (make-req:quit)))


;; delete-model: model -> void
;; Destroys the model.
(define (delete-model! a-model)
  (send-request a-model (make-req:quit))
  (delete-directory/files (model-data-dir a-model)))



(provide/contract [rename -make-model make-model (path-string? . -> . model?)]
                  [close-model (model? . -> . any)]
                  [delete-model! (model? . -> . any)]
                  
                  [add-user! (model? string? string? . -> . user?)]
                  [find-users (model? . -> . (listof user?))]
                  [find-user/email (model? string? . -> . (or/c user? false/c))])
