#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/path
         scheme/file
         scheme/list
         "../compile-world.ss"
         "../utils.ss")


;; The model we maintain has a maintenance thread, a request channel, and a response channel.
;; data-dir is a path that we use to store things like the database and other disk-based
;; resources.
(define-struct model (th       ;; maintenance thread
                      req-ch   ;; request channel
                      res-ch   ;; response channel
                      data-dir ;; absolute path
                      snooze
                      ) #:mutable)



(define-struct user (name  ;; string
                     email ;; string
                     ))


(define-struct source (id             ;; string
                       program-name   ;; string
                       code           ;; bytes
                       date-submitted ;; date
                       approved?      ;; boolean
                       ))


(define-struct binary (name ;; string
                       bytes ;; bytes
                       ))

;;;;



(define-struct platform ())
(define-struct (platform:j2me platform) ())
(define-struct (platform:android platform) ())

;;;;;;



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
    (install-tables!)
    (set-model-th! a-model 
                   (thread (lambda () 
                             (request-loop a-model))))
    a-model))


(define (install-tables!)
  (void))



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



;; compile-source: model source platform -> binary              
(define (compile-source a-model a-source a-platform)
  (with-serializing 
   a-model
   (lambda ()
     (printf "Here I am~n")
     (match a-platform
       [(struct platform:j2me ())
        (let* ([name (source-program-name a-source)]
               [dir (make-temporary-directory #:parent-directory (model-data-dir a-model))]
               [program-path (build-path dir (string-append name ".ss"))])
          (printf "about to write bytes~n")
          (call-with-output-file program-path 
            (lambda (op)
              (write-bytes (source-code a-source) op)))
          (printf "filename: ~s~n" program-path)
          (read)
          (printf "hello again~n")
          (generate-j2me-application name program-path dir)
          (let ([bin (make-binary
                      name 
                      (get-file-bytes (first (find-files jar-path? (build-path dir "bin")))))])
            #;(delete-directory/files dir)
            (display bin)
            (newline)
            bin))]
       [(struct platform:android ())
        (void)]))))

(define (jar-path? a-path)
  (and (filename-extension a-path)
       (regexp-match #rx".jar" (filename-extension a-path))))





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
                  [compile-source (model? source? platform? . -> . binary?)]
                  
                  

                  [struct source ((id string?)
                                  (program-name string?)
                                  (code bytes?)
                                  (date-submitted date?)
                                  (approved? boolean?))]

                  [struct binary ((name string?)
                                  (bytes bytes?))]
                  
                  [struct platform ()]
                  [struct (platform:j2me platform) ()]
                  [struct (platform:android platform) ()])
