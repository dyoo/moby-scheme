;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname signal-strengths) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (lib "moby.ss" "moby" "stub"))

;; The world is a list of towers.
(define-struct tower (id strength))

(define initial-world empty)

;; update: world -> world
;; Update all the signal strength readings.
(define (update w)
  (local [(define infos (get-signal-strengths))

          (define (info->tower an-info)
            (make-tower (first an-info)
                        (second an-info)))]
    (map info->tower infos)))


;; tower->string: tower -> string
;; Converts a tower into something readable.
(define (tower->string a-tower)
  (string-append (number->string (tower-id a-tower))
                 " "
                 (number->string (tower-strength a-tower))))
          

;; draw: world -> (sexpof dom)
(define (draw w)
  (local [(define (tower->node a-tower)
            (list (js-text (tower->string a-tower))))] 
  (list (js-p) 
        (list (js-text "Here are the following towers."))
        (cons (js-p)
              (map tower->node w))
        
        (list (js-button update) (list (js-text "Update!"))))))


;; draw-css: world -> (sexpof css)
(define (draw-css)
  '())

(js-big-bang initial-world
             '()
             (on-draw draw draw-css)
             (on-tick 30 update))