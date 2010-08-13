;; FIXME: still in implicit moby language level, but without the
;; necessary #lang...

(provide on-tick
         on-tick! 
         on-key 
         on-key!)

(define (on-tick! world-updater effect interval-seconds)
  (local [(define interval (* interval-seconds 1000))
          (define ticking (box #f))
          (define (set-tick run-tick)
            (js-call (js-get-global-value "setTimeout")
                     #f
                     (procedure->void-js-fun run-tick)
                     (scheme->prim-js interval)))
          (define (ticker a-bb-info)
            (lambda ()
              (if (unbox ticking)
                  (begin ((bb-info-change-world a-bb-info)
                          (lambda (w) (world-with-effects (effect w)
                                                          (world-updater w))))
                         (set-tick (ticker a-bb-info)))
                  (void))))]
    (make-world-config (lambda (a-bb-info)
                         (begin (set-box! ticking #t)
                                (set-tick (ticker a-bb-info))))
                       (lambda (junk) (set-box! ticking #f)))))


(define (on-tick world-updater interval-seconds)
  (local [(define interval (* interval-seconds 1000))
          (define ticking (box #f))
          (define (set-tick run-tick)
            (js-call (js-get-global-value "setTimeout")
                     #f
                     (procedure->void-js-fun run-tick)
                     (scheme->prim-js interval)))
          (define (ticker a-bb-info)
            (lambda ()
              (if (unbox ticking)
                  (begin ((bb-info-change-world a-bb-info) world-updater)
                         (set-tick (ticker a-bb-info)))
                  (void))))]
    (make-world-config (lambda (a-bb-info)
                         (begin (set-box! ticking #t)
                                (set-tick (ticker a-bb-info))))
                       (lambda (junk) (set-box! ticking #f)))))


(define (prevent-default e)
  (if (js-=== (js-get-field e "preventDefault") js-undefined)
      (js-set-field! e "returnValue" (scheme->prim-js #f))
      (js-call (js-get-field e "preventDefault") e)))

(define (stop-propagation e)
  (if (js-=== (js-get-field e "stopPropagation") js-undefined)
      (js-set-field! e "cancelBubble" (scheme->prim-js #t))
      (js-call (js-get-field e "stopPropagation") e)))

(define (attach-event node event-name f)
  (if (js-=== (js-get-field node "addEventListener") js-undefined)
      (js-call (js-get-field node "attachEvent")
               node
               (scheme->prim-js (string-append "on" event-name))
               f)
      (js-call (js-get-field node "addEventListener")
               node
               (scheme->prim-js event-name)
               f)))

(define (detach-event node event-name f)
  (if (js-=== (js-get-field node "removeEventListener") js-undefined)
      (js-call (js-get-field node "detachEvent")
               node
               (scheme->prim-js (string-append "on" event-name))
               f)
      (js-call (js-get-field node "removeEventListener")
               node
               (scheme->prim-js event-name)
               f)))

(define (get-key-code-name e)
  (let* ([code (inexact->exact (prim-js->scheme (js-get-field e "keyCode")))])
    (cond
      [(= code 16) "shift"]
      [(= code 17) "control"]
      [(= code 19) "pause"]
      [(= code 27) "escape"]
      [(= code 33) "prior"]
      [(= code 34) "next"]
      [(= code 35) "end"]
      [(= code 36) "home"]
      [(= code 37) "left"]
      [(= code 38) "up"]
      [(= code 39) "right"]
      [(= code 40) "down"]
      [(= code 42) "print"]
      [(= code 45) "insert"]
      [(= code 46) "\u007F"]
      [(= code 106) "*"]
      [(= code 107) "+"]
      [(= code 109) "-"]
      [(= code 110) "."]
      [(= code 111) "/"]
      [(= code 144) "numlock"]
      [(= code 145) "scroll"]
      [(= code 186) ";"]
      [(= code 187) "="]
      [(= code 188) ","]
      [(= code 189) "-"]
      [(= code 190) "."]
      [(= code 191) "/"]
      [(= code 192) "`"]
      [(= code 219) "["]
      [(= code 220) "\\"]
      [(= code 221) "]"]
      [(= code 222) "'"]
      [(and (>= code 96)
            (<= code 105))
       (number->string (- code 96))]
      [(and (>= code 112)
            (<= code 123))
       (string-append "f" (number->string (- code 111)))]
      [else (int->string code)])))

(define (on-key! world-updater effect)
  (make-world-config (lambda (a-bb-info)
                       (let ([wrapped-press
                              (procedure->void-js-fun
                               (lambda (e)
                                 (let ([keyname (get-key-code-name e)])
                                   (begin (prevent-default e)
                                          (stop-propagation e)
                                          ((bb-info-change-world a-bb-info)
                                           (lambda (w)
                                             (world-with-effects (effect w keyname)
                                                                 (world-updater w keyname))))))))])
                         (begin
                           (attach-event (bb-info-toplevel-node a-bb-info)
                                         "keydown"
                                         wrapped-press)
                           (lambda ()
                             (detach-event (bb-info-toplevel-node a-bb-info)
                                           "keydown"
                                           wrapped-press)))))
                     (lambda (shutdownF) (shutdownF))))

(define (on-key world-updater)
  (make-world-config (lambda (a-bb-info)
                       (let ([wrapped-press
                              (procedure->void-js-fun
                               (lambda (e)
                                 (let ([keyname (get-key-code-name e)])
                                   (begin (prevent-default e)
                                          (stop-propagation e)
                                          ((bb-info-change-world a-bb-info)
                                           (lambda (w) (world-updater w keyname)))))))])
                         (begin
                           (attach-event (bb-info-toplevel-node a-bb-info)
                                         "keydown"
                                         wrapped-press)
                           (lambda ()
                             (detach-event (bb-info-toplevel-node a-bb-info)
                                           "keydown"
                                           wrapped-press)))))
                     (lambda (shutdownF) (shutdownF))))