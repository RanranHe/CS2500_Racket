;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.17(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; c->f: Number -> Number 
;; convert a temperature in celcius to fashrenheit


(define (c->f t)
  (+ (* 9/5 t) 32))

(check-expect (c->f 0) 32)
(check-expect (c->f 100) 212)

;; A Rating is one of:
;; 'G
;; 'PG
;; 'PG13
;; 'R

;; Rating -> Boolean
;; determine if the rating is child-friendly
(define (child-friendly? r)
  (cond [(symbol=? r 'G) true]
        [(symbol=? r 'PG) true]
        [(symbol=? r 'PG13) true]
        [(symbol=? r 'R) false]))

(check-expect (child-friendly? 'R) false)
(check-expect (child-friendly? 'PG13) true)
(check-expect (child-friendly? 'PG) true)
(check-expect (child-friendly? 'G) true)
              