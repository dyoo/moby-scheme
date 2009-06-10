#lang scheme/base

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



(define-struct effect:none () 
  #:prefab)

(define-struct effect:send-sms (address  ;; string
                                msg ;; string
                                )
  #:prefab)

(define-struct effect:play-sound-url (url ;; string
                                  )
  #:prefab)


;; effect?: X -> boolean
;; Determines if thing is an effect.
(define (effect? thing)
  (or (effect:none? thing)
      (effect:send-sms? thing)
      (effect:play-sound-url? thing)))



;; effect-apply!: effect -> void
;; Apply
(define (effect-apply! e)
  (cond
    [(effect:none? e)
     (void)]
    
    [(effect:send-sms? e)
     (void)]

    [(effect:play-sound-url? e)
     (void)]))



(provide (all-defined-out))