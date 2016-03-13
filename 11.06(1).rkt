;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.06(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; S-exp S-exp -> Boolean
;; are the two s-exp equal?
#;(define (s-exp-temp exp)
    (cond [(atom? exp) ...(atom-temp exp)]
          [(list? exp) ...(los-temp exp)]))

;; [List-of S-exp] -> ?
#;(define (los-temp alos)
    (cond [(empty? alos) ...]
          [(cons? alos) ...(s-exp-temp (first alos))
                        ...(los-temp (rest alos))]))

(check-expect (s-exp=? '(4 (b "x") (5)) '(4 (b "x") (5))) true)
(check-expect (s-exp=? '(4 (b "x") (5)) '(4 (b 'x) (5))) false)

;; atom-in? : S-exp -> Boolean
(define (atom-in? a exp)
  (cond [(atom? exp) (atom=? a exp)]
        [(list? exp) (atom-in-list? a exp)]))

;; atom-in-list? : Atom [List-of S-exp] -> Boolean
(define (atom-in-list? a alos)
 (ormap (λ (x) (atom-in? a x)) alos))
  #;(cond [(empty? alos) false]
        [(cons? alos) (or (atom-in? a (first alos))
                          (atom-in-list? a (rest alos)))])

(check-expect (atom-in? 5 '(4 (b "x") (5))) true)
(check-expect (atom-in? 6 '(4 (b "x") (5))) false)
(check-expect (atom-in? 5 "b") false)
(check-expect (atom-in? 5 5) true)

;; Atom -> Boolean
(define (atom? a) (or (number? a) (string? a) (symbol? a)))

;; atom=? : Atom Atom -> Boolean
;; are the two atoms equal?
(define (atom=? a1 a2)
  (cond [(number? a1) (and (number? a2) (= a1 a2))]
        [(symbol? a1) (and (symbol? a2) (symbol=? a1 a2))]
        [(string? a1) (and (string? a2) (string=? a1 a2))]))

(check-expect (atom=? 'a 'a) true)
(check-expect (atom=? 1 "r") false)

;; S-exp S-exp -> Boolean
;; are the two s-exp equal?
(define (s-exp=? exp1 exp2)
  (cond [(and (atom? exp1) (atom? exp2)) (atom=? exp1 exp2)]
        [(and (list? exp1) (list? exp2)) (los=? exp1 exp2)]
        [else false]))

;; [List-of S-exp] -> ?
(define (los=? alos1 alos2)
  (cond [(and (empty? alos1) (empty? alos2)) true]
        [(and (cons? alos1) (cons? alos2)) 
         (and (s-exp=? (first alos1) (first alos2))
              (los=? (rest alos1) (rest alos2)))]
        [else false]))

(check-expect (s-exp=? '(4 (b "x") (5)) '(4 (b "x") (5))) true)
(check-expect (s-exp=? '(4 (b "x") (5)) '(4 (b 'x) (5))) false)
