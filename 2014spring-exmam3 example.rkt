;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2014spring-exmam3 example|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;==============================2014spring-exmam3 example==================================;;

;;================Problem 1================;;
;; We can make binary trees with strings for leaves, or binary trees with numbers for 
;; leaves, or binary trees with anything we’d like for the leaves, using the following data definition.

(define-struct node (left right))
;; A [BT X] is one of:
;; - an X
;; - (make-node [BT X] [BT X])

;; For example, we can make a binary tree of strings (i.e., a [BT String]) with
(define bt1 (make-node (make-node "Olin" "Shivers")
                       (make-node "David"
                                  (make-node "Van" "Horn"))))

;; Recall that the foldr operation allows us to process the elements of a list: add them up, 
;; multiply them together, assemble them into a set, etc. Similarly, we can define an analogous 
;; “loop function” to fold up a binary tree, called fold-tree.
;; Applying fold-tree with these arguments
;; (fold-tree + string-length bt1)
;; will replace every occurrence of make-node in bt1 with +, and every leaf s with
;; (string-length s), computing
;; (+ (+ (string-length "Olin") (string-length "Shivers"))
;; (+ (string-length "David")
;; (+ (string-length "Van") (string-length "Horn"))))

;;=====Question 1=====;;
;;  fold-tree : [Y Y -> Y] [X -> Y] [BT X] -> Y
(define (fold-tree op1 op2 bt)
  (cond [(node? bt) (op1 (fold-tree op1 op2 (node-left bt)) 
                         (fold-tree op1 op2 (node-right bt)))]
        [else (op2 bt)]))

(check-expect (fold-tree + string-length bt1) 23)

;;=====Question 2=====;;
;; Use fold-tree to define the height function, which produces the height of a tree

;; tree-height : 
;; produces the height of a tree
(define (tree-height bt)
  (fold-tree (λ (left-height right-height)
               (add1 (max left-height right-height)))
             (λ (x) 0)
             bt))
#;(cond [(node? bt) (op1 (fold-tree op1 0 (node-left bt)) 
                         (fold-tree op1 0 (node-right bt)))]
        [else (op2 bt)])

#;(cond [(node? bt) (max (+ 1 (tree-height (node-left bt)))
                         (+ 1 (tree-height (node-right bt))))]
        [else 0])

(check-expect (tree-height 10) 0)
(check-expect (tree-height (make-node "tree" 
                                      (make-node "good" 
                                                 (make-node 10 20)))) 3)
(check-expect (tree-height (make-node (make-node "good" 
                                                 (make-node 10 20))
                                      "tree")) 3)

;;================Problem 2================;;
;; a Monthly-Report is a (make-monthly-report string number number number)
;; where month is the current month
;; year is the current year
;; expenditures is the total expenditures for the month
;; revenues is the total revenues
(define-struct monthly-report 
  (month year expenditures revenues))

;; A [Listof Monthly-Report] is one of
;; --empty
;; --(cons Monthly-Report [Listof Monthly-Report])

;; Design a function total-profits that computes the total profits for a given year, 
;; where Profit = Revenue – Expenditures
;; Your function must use an accumulator

;; total-profits : Number [Listof Monthly-Report] -> Number
;; consumes a list of monthly budget reports and a year and 
;; computes the total profits for that year
(define (total-profits y lom)
  (local [(define this-year (filter (λ (r) (equal? y (monthly-report-year r))) lom))]
  (cond [(empty? lom) 0]
        [else (- (foldr + 0 (map monthly-report-revenues this-year))
                 (foldr + 0 (map monthly-report-expenditures this-year)))])))

(define lom1 (list (make-monthly-report "May" 2014 10 20)
                   (make-monthly-report "June" 2014 25 50)
                   (make-monthly-report "June" 2013 25 50)))

(check-expect (total-profits 2014 empty) 0)
(check-expect (total-profits 2014 lom1) 35)
(check-expect (total-profits 2013 lom1) 25)
(check-expect (total-profits -2 lom1) 0)

;;================Problem 3================;;
;; Design a function, guess-number, that consumes two integers lo and hi, and produces the number 
;; stored in target. Assume that lo < hi, and that target is an integer in the range [lo, hi]. Your 
;; function must be efficient to get credit
;; it should only make at most about 20 guesses in order to find a 
;; number in the range [0, 1000000].

(define target 20)
;; guess-number : Integer Integer -> Integer
;; consumes two integers lo and hi, and produces the number 
(define (guess-number lo hi)
  (local [(define mid (quotient (+ lo hi) 2))]
    (cond [(= target mid) mid]
          [(< target mid) (guess-number lo (sub1 mid))]
          [(> target mid) (guess-number (add1 mid) hi)])))

(check-expect (guess-number 0 1000000) 20)

;;================Problem 4================;;
;; Design lex<, the comparison function that takes two lists of numbers as inputs, and determines 
;; if its first argument is lexicographically less than its second argument

;; lex< : [List-of X] [List-of X] -> Boolean
;; takes two lists of numbers as inputs, and determines 
;; if its first argument is lexicographically less than its second argument
(define (lex< lon1 lon2)
  (cond [(and (empty? lon1) (empty? lon2)) false]
        [(and (cons? lon1) (empty? lon2)) false]
        [(and (empty? lon1) (cons? lon2)) true]
        [else (cond [(< (first lon1) (first lon2)) true]
                    [(> (first lon1) (first lon2)) false]
                    [(= (first lon1) (first lon2))
                     (lex< (rest lon1) (rest lon2))])]))

(check-expect (lex< empty empty) false)
(check-expect (lex< '() '(1)) true)
(check-expect (lex< '(1) '()) false)
(check-expect (lex< '(1) '(1)) false)
(check-expect (lex< '(1) '(1 2)) true)
(check-expect (lex< '(10 2 3 4 5 6 7 8 9) '(1 2)) false)
(check-expect (lex< '(1 2 3 4 5 6 7 8 9) '(1 2)) false)
(check-expect (lex< '(1 2 3 4 5 6 7 8 9) '(1 2 4)) true)

