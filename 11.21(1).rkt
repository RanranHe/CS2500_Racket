;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.21(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A value is one of : Number String

;; A Prim is one of :
;; '+, '-, '*, '/, 'sqrt
;; 'string-length, 'string-append, 'number->string

;; PExp (primitive expression) is one of :
;;value
;;PApp

;; A PApp os a (cons Prim [List-of PExp])

(define prims (list
               (list '+ +) (list '- -) (list '* *) (list '/ /) (list 'sqrt sqrt)
               (list 'string-length string-length)
               (list 'string-append string-append)
               (list 'number->string number->string)))

;; prim? : Any -> Boolean
;; is the argument a primitive?
(define (prim? p)
  (if (list? (assq p primes)) true false))

(check-expect (prim? '- ) true)
(check-expect (prim? '=) false)

;; prim-eval : PExp -> Value
;; evaluate a primitive exp
(define (prim-eval aexp)
  (cond [(or (number? aexp) (string? aexp)) aexp]
        [else (apply-prim (first aexp) (prim-eval (rest aexp)))]))