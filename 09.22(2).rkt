;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.22(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A shape is one of:
;; Circl
;; Squar

;; A circle is a (make-circle Posn Number)
(define-struct circl (center radius))

;; A squar is (make-squar Posn Number)
(define-struct squar (nw side))

(define sh1 (make-circl (make-posn 40 60) 30))
(define sh2 (make-squar (make-posn 20 60) 25))

;; shape -> ?
(define (perimeter sh)
  (cond [(circl? sh) (* 2 3.14 (circl-radius sh))]
        [(squar? sh) (* 4 (squar-side sh))]))

;; perimeter: Shape -> Number
;; compute the perimeter
(check-expect (perimeter sh2) 100)
(check-expect (perimeter sh1) ( * 60 3.14))
