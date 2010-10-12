#lang scheme/base

(require "generate-application.ss" 
         "config.ss"
         scheme/runtime-path)

(define (start-up-debug-printing)
  (let ([receiver (make-log-receiver (current-logger) 
                                     'debug
                                     #;'info
                                     #;'error)])
    (thread (lambda ()
               (let loop ()
                 (let ([v (sync receiver)])
                   (display (vector-ref v 1))
                   (newline))
                 (loop))))
    receiver))
(define receiver (start-up-debug-printing))

;; The regressing test programs live here:
(define-runtime-path test-path "test")


;; Applications will be written to bin.
(define-runtime-path test-app-j2me-path "bin/j2me")
(define-runtime-path test-app-android-path "bin/android")
(define-runtime-path test-app-js-path "bin/js")


;; make-test: string -> void
;; Builds the test application.
(define (make-test filename)
  (lambda (generator where)
    (generator
     (regexp-replace #rx".(ss|scm)$" (format "~a" filename) "")
     (build-path test-path filename)
     (build-path where (regexp-replace #rx".(ss|scm)$" (format "~a" filename) "")))))


;; Here are a few small examples that exercise small portions of the compiler.
(define test-falling-ball (make-test "falling-ball.ss"))
(define test-falling-cow (make-test "falling-cow.ss"))
(define test-falling-ball-posn (make-test "falling-ball-posn.ss"))
(define test-falling-ball-pair (make-test "falling-ball-pair.ss"))
(define test-pinholes (make-test "pinholes.ss"))
(define test-rectangles (make-test "rectangles.ss"))
(define test-hello-world (make-test "hello-world.ss"))
(define test-approx-equal (make-test "approx-equal.ss"))
(define test-struct-question (make-test "struct-question.ss"))
(define test-move-ball (make-test "move-ball.ss"))
(define test-image-question (make-test "image-question.ss"))
(define test-location (make-test "location.ss"))
(define test-location-2 (make-test "location-2.ss"))
(define test-tilt (make-test "tilt.ss"))
(define test-bubble (make-test "bubble.ss"))
(define test-sketch (make-test "sketch.ss"))
(define test-sketch-2 (make-test "sketch-2.ss"))
(define test-upside-down (make-test "upside-down.ss"))
(define test-bubble-2 (make-test "bubble-2.ss"))
(define test-bubble-3 (make-test "bubble-3.ss"))
(define test-sms (make-test "sms.ss"))
(define test-homeward-bound (make-test "homeward-bound.ss"))
(define test-homeward-bound-single (make-test "homeward-bound-single.ss"))
(define test-net (make-test "net.ss"))
(define test-get-ip-address (make-test "get-ip-address.ss"))
(define test-parse-google-maps-places (make-test "parse-google-maps-places.ss"))
(define test-grocery-shopper (make-test "grocery-shopper.ss"))
(define test-local (make-test "local.ss"))

(define test-gui-world-hello-world (make-test "gui-world-hello-world.ss"))
(define test-gui-world-drop-down (make-test "gui-world-drop-down.ss"))
(define test-gui-world-text-field (make-test "gui-world-text-field.ss"))
(define test-gui-world-box-group (make-test "gui-world-box-group.ss"))
(define test-gui-world-checkbox (make-test "gui-world-checkbox.ss"))
(define test-gui-world-location (make-test "gui-world-location.ss"))

(define test-simple-bootstrap-game (make-test "simple-bootstrap-game.ss"))

;; The programs here are the five programs of
;; How To Design Worlds (http://world.cs.brown.edu)
(define test-cowabunga (make-test "cowabunga.ss"))
(define test-flight-lander (make-test "flight-lander.ss"))
(define test-chicken (make-test "chicken.ss"))
(define test-fire-fighter (make-test "fire-fighter.ss"))
(define test-spaceflight (make-test "spaceflight.ss"))


;; Exercises the application generator.
(define (test-all g w)
  (for ([test (list test-hello-world
                    test-falling-ball 
                    test-falling-cow 
                    test-falling-ball-posn 
                    test-falling-ball-pair 
                    test-pinholes 
                    test-rectangles 
                    test-approx-equal 
                    test-struct-question
                    test-move-ball
                    test-image-question
                    test-local
                    
                    test-location
                    test-location-2
                    test-tilt
                    test-bubble
                    test-sketch
                    test-sketch-2
                    test-upside-down
                    test-bubble-2
                    test-bubble-3
                    test-sms
                    test-homeward-bound
                    test-homeward-bound-single
                    
                    ;; Commented out: uses deprecated gui-world
                    #;test-net
                    #;test-get-ip-address
                    #;test-parse-google-maps-places
                    #;test-grocery-shopper
                    #;test-gui-world-hello-world
                    #;test-gui-world-drop-down
                    #;test-gui-world-text-field
                    #;test-gui-world-box-group
                    #;test-gui-world-checkbox
                    
                    test-cowabunga
                    test-flight-lander
                    test-chicken
                    test-fire-fighter
                    test-spaceflight
                    
                    test-simple-bootstrap-game)])
    (test g w)))


(define (run-all-tests #:android (with-android? #f)
                       #:js (with-js? #t))

  (define should-run-android?
    (and (directory-exists? (current-android-sdk-path)) with-android?))

  ;; Compile with javascript backend.
  (when with-js?
    (test-all generate-javascript-application test-app-js-path))
  
  ;; If you do not have the Android SDK, change the value in config.ss.
  #;(when should-run-android?
    (test-all generate-javascript+android-phonegap-application test-app-android-path)))



(define (run-single-test a-test 
                         #:android (with-android? #f)
                         #:js (with-js? #t))
  (define should-run-android?
    (and (directory-exists? (current-android-sdk-path)) with-android?))

  (when with-js?
    (a-test generate-javascript-application test-app-js-path))
  #;(when should-run-android?
    (a-test generate-javascript+android-phonegap-application test-app-android-path)))


(run-all-tests)