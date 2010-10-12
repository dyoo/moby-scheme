#lang scheme/base
(require lang/prim
         scheme/port
         net/url)

(define (get-url a-url-string)
  (with-handlers ((exn:fail? (lambda (exn) "")))
    (let ([ip (get-pure-port (string->url a-url-string))]
          [op (open-output-string)])
      (copy-port ip op)
      (close-input-port ip)
      (close-output-port op)
      (get-output-string op))))
    
            

(provide-primitive get-url)