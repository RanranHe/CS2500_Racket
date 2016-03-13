;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.19|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define rel (list 0 50 40 70 30 30))
(define abs-list (list 0 50 90 160 190 220))

;; rel->abs : [List-of Number] -> [List-of Number]
;; convert relative distances to absolute
(define (rel->abs alon)
  (cond [(empty? alon) empty]
        [else (cons (first alon)
                    (add-to-each (first alon)
                                 (rel->abs (rest alon))))]))

(check-expect (rel->abs rel) abs-list)

;; add-to-each : Number [List-of Number] -> [List-of Number]
;; add the number to each member of the list
(define (add-to-each n alon)
  (map (λ (x) (+ n x)) alon))

(check-expect (add-to-each 3 empty) empty)
(check-expect (add-to-each 3 '(1 2)) '(4 5))
  
;; rel-abs.v2 : [List-of Number] Number -> [List-of Number]
;; accumulutor: keeps track of the distance traveled so far
(define (rel-abs.v2 alon acc)
  (cond [(empty? alon) empty]
        [else (cons (+ (first alon) acc)
                    (rel-abs.v2 (rest alon)
                                (+ (first alon) acc)))]))

(check-expect (rel-abs.v2 rel 0) abs-list)

;; rel-abs.v3 : [List-of Number] -> [List-of Number]
;; accumulutor: keeps track of the distance traveled so far
(define (rel-abs.v3 alon)
  (local [;; accumulutor: keeps track of the distance traveled so far
          (define (r alon acc)
            (cond [(empty? alon) empty]
                  [else (cons (+ (first alon) acc)
                              (r (rest alon) (+ (first alon) acc)))]))]
    (r alon 0)))


;; fact : Number -> Number
;; compute the factorial of n
(define (fact n)
  (cond [(= n 1) 1]
        [else (* n (fact (- n 1)))]))

(define-struct node (left right))
;; A BT is one of :
;; Number
;; (make-node BT BT)

(define (sum abt)
  (local [;; accumulator 

;; fact2 : Number -> Number
;; compute the factorial of n
(define (fact2 n)
  (local [;;accumulutor: keeps track of n-1 factorial
          (define (f n acc)
            (cond [(= n 1) acc]
                  [else (f (- n 1) (* n acc))]))]
    (f n 1)))