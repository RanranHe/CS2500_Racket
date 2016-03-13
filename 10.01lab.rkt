;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.01lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sum the umber in
;; a hand rolled list of number; using the design recipe

(define-struct empty-lon ())
(define-struct cons-lon (left right))
;; A HRLON (Hand-rolled List of Numbers) is one of:
;; (make-empty-lon)
;; (make-cons-lon Number HRLON)

;; examples
(define hrlon1 (make-empty-lon))
(define hrlon2 (make-cons-lon 1 hrlon1))
(define hrlon3 (make-cons-lon 2 (make-cons-lon 1 (make-empty-lon))))

;; template
;; hrlon-template: HRLON -> Any
(define (hrlon-template a-hrlon)
  (cond [(empty-lon? a-hrlon) ...]
        [(cons-lon? a-hrlon)
         ... (cons-lon-left a-hrlon) ...
         ... (hrlon-templete (cons-lon-right a-hrlon)) ...]))

;; hrlon-sum: HRLON -> Number
;; sum the numbers in a hand rolled list of numbers
(define (hrlon-sum a-hrlon)
  (cond [(empty-lon? a-hrlon) 0] ; base case
        [(cons-lon? a-hrlon)
         (+ (cons-lon-left a-hrlon) ...
            (hrlon-templete (cons-lon-right a-hrlon)))]))

(check-expect (hrlon-sum hrlon1) 0)
(check-expect (hrlon-sum hrlon2) 1)
(check-expect (hrlon-sum hrlon3) 3)
