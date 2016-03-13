;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.15(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
;; A ring is a (make-ring Number Number)
;; inner must be less than outer
(define-struct ring (outer inner))

(define r1 (make-ring 400 300))
(define r2 (make-ring 50 30))

;; ring -> image
;; draw the ring
(define (draw-ring r)
  (overlay (circle (ring-inner r) "solid" "white")
           (circle (ring-outer r) "solid" "blue")))

(check-expect (draw-ring r1)
              (overlay (circle 300 "solid" "white")
                       (circle 400 "solid" "blue")))