#lang scheme/base

(require "../collects/moby/runtime/effect-struct.ss"
         scheme/contract
         scheme/port
         scheme/path
         scheme/list
         (only-in scheme/gui/base play-sound)
         net/url)

;                                                                        
;                                                                        
;                                                                        
;   ;;;;;;;;      ;;;       ;;;                                          
;   ;            ;         ;                            ;                
;   ;            ;         ;                            ;                
;   ;          ;;;;;;    ;;;;;;     ;;;;      ;;;;    ;;;;;;      ;;;;;  
;   ;            ;         ;       ;    ;    ;    ;     ;       ;;     ; 
;   ;;;;;;;;     ;         ;      ;      ;  ;           ;       ;        
;   ;            ;         ;      ;      ;  ;           ;       ;;       
;   ;            ;         ;      ;;;;;;;;  ;           ;        ;;;;;;  
;   ;            ;         ;      ;         ;           ;             ;; 
;   ;            ;         ;      ;         ;           ;              ; 
;   ;            ;         ;       ;     ;   ;    ;     ;       ;     ;; 
;   ;;;;;;;;     ;         ;        ;;;;;     ;;;;       ;;;     ;;;;;   
;                                                                        
;                                                                        
;                                                                        
;                                                                        


;; lookup-sound-url: string -> bytes
(define lookup-sound-url
  (let ([sound-ht (make-hash)])
    
    (lambda (url-string)
      (define (get-port-bytes ip)
        (let ([op (open-output-bytes)])
          (copy-port ip op)
          (close-input-port ip)
          (close-output-port op)
          (get-output-bytes op)))
      (hash-ref sound-ht url-string
                (lambda ()
                  (let ([sound-bytes 
                         (get-port-bytes
                          (get-pure-port (string->url url-string)))])
                    (hash-set! sound-ht url-string sound-bytes)
                    sound-bytes))))))



;; effect-apply!: effect -> void
;; Apply
(define (effect-apply! e)
  (cond
    [(effect:none? e)
     (void)]
    
    [(effect:beep? e)
     ;; fixme: how do we beep?
     (void)]
    
    [(effect:play-dtmf-tone? e)
     (void)]
   
    [(effect:send-sms? e)
     ;; fixme
     (void)]
    
    [(effect:play-sound? e)
     (void)
     
     #;(unless (regexp-match #rx"\\.wav$" 
                           (string-downcase 
                            (effect:play-sound-sound e)))
       (error 'play-sound "Only supports .wav at the moment."))
     #;(let* ([url-string (effect:play-sound-url-url e)]
            [sound-bytes (lookup-sound-url url-string)]
            
            [filename (make-temporary-file 
                       (string-append
                        "mzsoundtmp~a."
                        (file-name-from-url-string url-string)))]
            [async? #f])
       (dynamic-wind (lambda ()
                       (call-with-output-file filename
                         (lambda (op)
                           (write-bytes sound-bytes op))
                         #:exists 'truncate))
                     (lambda ()
                       (play-sound filename async?))
                     (lambda ()
                       (delete-file filename))))]
    [(effect:pause-sound? e)
     (void)]
    [(effect:stop-sound? e)
     (void)]
    [(effect:set-sound-volume? e)
     (void)]
    [(effect:raise-sound-volume? e)
     (void)]
    [(effect:lower-sound-volume? e)
     (void)]
    [(effect:set-wake-lock? e)
     (void)]
    [(effect:release-wake-lock? e)
     (void)]
    [(effect:pick-playlist? e)
     (void)]
    [(effect:pick-random? e)
     ((effect:pick-random-update-f e) (random (effect:pick-random-n e)))]))
     

;; file-name-from-url-string: string -> string
;; Tries to get the file name from the url string.
(define (file-name-from-url-string url-string)
  (path->string 
   (file-name-from-path 
    (path/param-path 
     (last (url-path (string->url url-string)))))))
                        



(provide/contract [effect-apply! (effect? . -> . any)])
