;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define WIDTH 320)
(define HEIGHT 480)

(define the-world 42)

(send-text-message "5556"
                   "This is a test; hello world"
                   the-world)

(big-bang WIDTH HEIGHT the-world)