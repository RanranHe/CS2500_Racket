;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2011fall exame2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;=================2011fall exame2======================;;

;;================Problem 1================;;
;; A Rectangle is a (make-rect Number Number)
(define-struct rect (width height))

;;=====Question 1=====;;
;; Design a function, all-squares, that takes a list of rectangles, 
;; and produces a list of all the square rectangles in the lists.
;; Do not use loop function

;; all-squares : [List-of Rectangles] -> [List-of Rectangles] 
;; takes a list of rectangles, and produces a list of 
;; all the square rectangles in the lists
(define (all-squares lor)
  (cond [(empty? lor) empty]
        [else (if (= (rect-width (first lor))
                     (rect-height (first lor)))
                  (cons (first lor) (all-squares (rest lor)))
                  (all-squares (rest lor)))]))

(check-expect (all-squares empty) empty)
(check-expect (all-squares (list (make-rect 20 20)
                                 (make-rect 10 20)
                                 (make-rect 20 40)
                                 (make-rect 40 40)))
              (list (make-rect 20 20)
                    (make-rect 40 40)))

;;=====Question 2=====;;
;; Now rewrite the function using loop functions.

;; all-squares : [List-of Rectangles] -> [List-of Rectangles] 
;; takes a list of rectangles, and produces a list of 
;; all the square rectangles in the lists
(define (all-squares0 lor)
  (filter (λ (r) (= (rect-width r) (rect-height r))) lor))

(check-expect (all-squares0 empty) empty)
(check-expect (all-squares0 (list (make-rect 20 20)
                                  (make-rect 10 20)
                                  (make-rect 20 40)
                                  (make-rect 40 40)))
              (list (make-rect 20 20)
                    (make-rect 40 40)))

;;================Problem 2================;;
;; We need a function, flip-rectangles, that will take a list of rectangles
;; and "flip" each one, swaping its width with its height.

;; flip-rectangles : [List-of Rectangles] -> [List-of Rectangles]
;; take a list of rectangles
;; and "flip" each one, swaping its width with its height
(define (flip-rectangles lor)
  (map (λ (r) (make-rect (rect-height r) (rect-width r))) lor))

(check-expect (flip-rectangles empty) empty)
(check-expect (flip-rectangles (list (make-rect 20 20)
                                     (make-rect 10 20)
                                     (make-rect 20 40)
                                     (make-rect 40 40)))
              (list (make-rect 20 20)
                    (make-rect 20 10)
                    (make-rect 40 20)
                    (make-rect 40 40)))

;;================Problem 3================;;
;; The function number-winners takes a list and a test function, and
;; returns the number of “winners” in the list—that is, the number of items 
;; in the input list that cause the test function to return true.
;; Some examples:
;(number-winners even? (list 2 5 2 7 9 1 4)) ; => 3
;(number-winners string? (list 0 "a" 3 1 "b" 7)) ; => 2

;; Design this function using any loop function you like. . . except filter.

;; number-winners : [X -> Boolean] [List-of X] -> Number
;; takes a list and a test function, and
;; returns the number of “winners” in the list
(define (number-winners op lox)
  (local [(define count (λ (x) (if (boolean=? x true) 1 0)))]
    (foldr + 0 (map count (map op lox)))))

(check-expect (number-winners even? (list 2 5 2 7 9 1 4)) 3)
(check-expect (number-winners string? (list 0 "a" 3 1 "b" 7)) 2)
(check-expect (number-winners odd? empty) 0)

;;================Problem 4================;;
;; Recall from class our representation of numeric sets with lists:
;;; An NSet (set of numbers) is a [Listof Number]
;;; - Order of elements is unimportant, of course.
;;; - No repeats allowed: a number may appear in the list at most once.

;;; NSet Number -> Boolean
;;; Does the set contain the number?
(define (contains? set num)
  (ormap (λ (x) (= x num)) set))

(check-expect (contains? '(1 2 3 4 5 6 7 8 9) 9) true)
(check-expect (contains? '(1 2 3 4 5 6 7 8 9) 10) false)
(check-expect (contains? empty 9) false)

;;================Problem 5================;;
;; Design map and filter using foldr. You may use lambda or
;; local, if needed

;; foldr : [X Y -> Y] Y [List-of X] -> [List-of Y]

;; map0 : [X -> Y] [List-og X] -> [List-of Y]
(define (map0 op lis)
  (foldr (λ (x y) (cons (op x) y)) '() lis))

(check-expect (map0 add1 '(1 2 3)) '(2 3 4))
(check-expect (map0 (λ (x) (+ x 1)) '(1 2 3)) '(2 3 4))
(check-expect (map0 odd? '(2 3 4 5 6)) (list false true false true false))

;; filter0 : [X -> Boolean] [List-of X] -> [List-of X]
(define (filter0 op lis)
  (foldr (λ (x y) (if (op x) (cons x y) y)) '() lis))

(check-expect (filter0 odd? '(1 2 3 4)) '(1 3))
(check-expect (filter0 (λ (x) (= x 10)) '(2 8 3 9)) '())
 
;;================Problem 6================;;
;; A Natural is one of:
;; - 0
;; - (add1 Natural)
(define (nat-foldr op base nat)
  (cond [(zero? nat) base]
        [else (op nat
                  (nat-foldr op base (sub1 nat)))]))

;; factorial : Natural -> Natural
;; Compute n! = 1 * 2 * ... * n-1 * n.
;; (As a special case, 0! = 1.)
(define (factorial n)
  (cond [(zero? n) 1]
        [else (* n
                 (factorial (sub1 n)))]))
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)
(check-expect (factorial 20) 2432902008176640000)

;;=====Question 1=====;;
;; nat-foldr : 
;; [Natural Natural -> Natural] Natural Natural -> [List-of Natural]

;;=====Question 2=====;;
(define (factorial0 n)
  (nat-foldr * 1 n))

(check-expect (factorial0 0) 1)
(check-expect (factorial0 5) 120)
(check-expect (factorial0 20) 2432902008176640000)

;;================Problem 7================;;
;; An Exp (arithmetic expression) is one of:
;; - Number
;; - (make-op Exp [Number Number -> Number] Exp)
(define-struct op (left fun right))

;;=====Question 1=====;;
;; Translate the following Intermediate Student Language expressions into Exps:
;; (+ 1 2) => (make-op 1 + 2)
;; (+ (* 2 3) 4) => (make-op (make-op 2 * 3) + 4)
;; (* (* 3 2) (+ 2 3)) => (make-op (make-op 3 * 2) * (make-op 2 + 3))

;;=====Question 2=====;;
;; Design a function, evaluate, that will take an Exp and carry out the 
;; arithmetic computation describes

;; evaluate : Exp -> Number
;; take an Exp and carry out the arithmetic computation describes
(define (evaluate exp)
  (cond [(number? exp) exp]
        [else ((op-fun exp) (evaluate (op-left exp))
                            (evaluate (op-right exp)))]))

(check-expect (evaluate 3) 3)
(check-expect (evaluate (make-op 1 + 2)) 3)
(check-expect (evaluate (make-op (make-op 2 * 3) + 4)) 10)
(check-expect (evaluate (make-op (make-op 3 * 2) * (make-op 2 + 3))) 30)