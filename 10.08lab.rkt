;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.08lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A (list of nuber) is one of :
;; empty
;; (cons Number (list-of Number))

;; lon-add2: list of Number -> List of Number
;; Add 2 to each element of the given list of numbers
#|
(define (lon-add2 lon)
  (cond [(empty? lon) empty]
        [else (cons (+ (first lon) 2)
                    (lon-add2 (rest lon)))]))

;; lon-add5: list of Number -> List of Number
;; Add 5 to each element of the given list of number
(define (lon-add5 lon)
  (cond [(empty? lon) empty]
        [else (cons (+ (first lon) 5)
                    (lon-add5 (rest lon)))]))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 1

;; a lom-add-n is one of :
;; empty
;; n(n is a Number which added to each element)
(define (lon-add-n lon n)
  (cond [(empty? lon) empty]
        [(cons? lon) (cons (+ (first lon) n)
                           (lon-add-n (rest lon) n))]))

;; Exercise 2
(define (lon-add2 lon)
  (lon-add-n lon 2))

(define (lon-add5 lon)
  (lon-add-n lon 5))

(check-expect (lon-add2 (cons 2 (cons 5 empty))) (cons 4 (cons 7 empty)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 3

;; A [List-of String] is one of
;; – empty
;; – (cons String [List-of String])

;; a start-with-cat is one of:
;; empty
;; list of strings that staart with "cat"
(define (start-with-cat st)
  (cond [(empty? st) empty]
        [(< (string-length (first st)) 3) (start-with-cat (rest st))]
        [(string=? (substring (first st) 0 3) "cat")
         (cons (first st) (start-with-cat (rest st)))]
        [else (start-with-cat (rest st))]))

(check-expect (start-with-cat (cons "ji" empty)) empty)
(check-expect (start-with-cat empty) empty)
(check-expect (start-with-cat 
               (cons "cat likes fish"
                     (cons "dog likes meat"
                           (cons "cat is small"
                                 (cons "the weather is great" empty)))))
               (cons "cat likes fish"
                     (cons "cat is small" empty)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 4

;; a start-with-cat is one of:
;; empty
;; list of strings that staart with "cat"
(define (start-with-dog st)
  (cond [(empty? st) empty]
        [(< (string-length (first st)) 3) (start-with-dog (rest st))]
        [(string=? (substring (first st) 0 3) "dog")
         (cons (first st) (start-with-dog (rest st)))]
        [else (start-with-dog (rest st))]))

(check-expect (start-with-dog (cons "ji" empty)) empty)
(check-expect (start-with-dog empty) empty)
(check-expect (start-with-dog 
               (cons "cat likes fish"
                     (cons "dog likes meat"
                           (cons "dogeghk"
                                 (cons "the weather is great" empty)))))
               (cons "dog likes meat"
                     (cons "dogeghk" empty)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 5

;; wo is a String
;; start-with is one of:
;; empty
;; list of string taht start with wo
(define (start-with wo st)
  (cond [(empty? st) empty]
        [(< (string-length (first st)) 3) (start-with wo (rest st))]
        [(string=? (substring (first st) 0 3) wo)
         (cons (first st) (start-with wo (rest st)))]
        [else (start-with wo (rest st))]))
               

(check-expect (start-with "dog" empty) empty)
(check-expect (start-with "dog"
               (cons "cat likes fish"
                     (cons "dog likes meat"
                           (cons "dogeghk"
                                 (cons "the weather is great" empty)))))
               (cons "dog likes meat"
                     (cons "dogeghk" empty)))
(check-expect (start-with "dog" (cons "DO" empty)) empty)
#|
;; rewrite start-with-cat
(define (start-with-cat st)
  (start-with "cat" st))

;; rewrite start-with-dog
(define (start-with-dog st)
  (start-with "dog" st))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 6

; A Nat, or natural number, is one of:
;  - 0
;  - (add1 Nat)
; 
; The predicate for 0 is: zero?
; 
; The predicate for (add1 n): positive?
; The selector for (add1 n): sub1

; temp-nat : Nat -> Any
#;(define (temp-nat n)
  (cond [(zero? n) ...]
        [(positive? n) 
         ... (temp-nat (sub1 n)) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 7

;; Nat -> Boolean
;; is a nat even?
(define (nat-even? n)
  (cond [(zero? n) "even"]
        [else 
               




