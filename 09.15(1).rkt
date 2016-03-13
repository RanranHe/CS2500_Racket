;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.15(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

(define (change t1)
  (cond [(string=? t1 "red") "green"]
        [(string=? t1 "green") "yellow"]
        [(string=? t1 "yellow") "red"]))

(check-expect (change "red") "green")

;; Trafficlight -> Trafficlight
;;change the light according to traffic rules
(define (draw t1)
  (circle 100 "solid" t1))

(define (main t1)
  (big-bang t1
            [on-tick change]
            [to-draw draw]))
            