;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.22(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A train is one of:
;; Subway
;; Amtrak
;; Commuter

;; A subway is (make-subway Number Number Symbol)
(define-struct subway (cars passengers color))

;; An Amtrak is (make-amtrak Number Number Symbol)
(define-struct amtrak (cars passengers type))

;; Acommuter is a (make-comm Number Number Boolean)
(define-struct comm (cars passengers express?))

(define t1 (make-subway 6 7 'red))
(define t2 (make-comm 8 50 true))

;; capacity: Train -> Number
;; compute the number of people train can carry
(define (capacity tr)
  (cond [(subway? tr) (* (subway-cars tr)  
                         (subway-passengers tr))]
        [(amtrak? tr) (* (amtrak-cars tr)
                         (amtrak-passengers tr))]
        [(comm? tr) (* (comm-cars tr)
                       (comm-passengers tr))]))
        

(check-expect (capacity t1) 42)
(check-expect (capacity t2) 400)