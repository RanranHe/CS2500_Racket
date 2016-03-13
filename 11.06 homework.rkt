;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.06 homework|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 9
;; A BTN is one of
;; - Number
;; - (make-node BTN BTN)
(define-struct node (left right))

;;Problem 1

;; btn-height: BTN -> Natural
;; takes in a binary tree of numbers and computes
;; the maximum distance from the root to any leaf
(define (btn-height btn)
  (cond [(number? btn) 0]
        [(node? btn) (if (> (add1 (btn-height (node-left btn)))
                            (add1 (btn-height (node-right btn))))
                         (add1 (btn-height (node-left btn)))
                         (add1 (btn-height (node-right btn))))]))

(check-expect (btn-height 42) 0)
(check-expect (btn-height (make-node 2 (make-node 4 9))) 2)
(check-expect (btn-height (make-node (make-node 4 9) (make-node 4 9))) 2)
(check-expect (btn-height (make-node (make-node 4 9) 2)) 2)
(check-expect (btn-height
               (make-node (make-node 4 (make-node 4 9)) (make-node 4 9))) 3)


;; Problem 2
;; btn-sum: BTN -> Natural
;; takes in a binary tree of numbers 
;; and computes the sum of all leaves
(define (btn-sum btn)
  (cond [(number? btn) btn]
        [(node? btn) (+ (btn-sum (node-left btn)) (btn-sum (node-right btn)))]))


(check-expect (btn-sum 42) 42)
(check-expect (btn-sum (make-node 2 (make-node 4 9))) 15)
(check-expect (btn-sum (make-node (make-node 4 9) (make-node 4 9))) 26)
(check-expect (btn-sum (make-node (make-node 4 9) 2)) 15)
(check-expect (btn-sum 
               (make-node (make-node 4 (make-node 4 9)) (make-node 4 9))) 30)

;; Problem 3
;; btn-height2 : BTN -> Natural
;; takes in a binary tree of numbers and computes
;; the maximum distance from the root to any leaf
(define (btn-height2 btn)
  (cond [(node? btn) 
         (add1 (max (btn-height2 (node-left btn))
                    (btn-height2 (node-right btn))))]
        [else 0]))

;; whole-tree : [List-of Tree] [List-of Tree] -> [List-of Tree]
;; mix the two list and create a new list
(define (whole-tree lisa lisb)
  (foldr append empty
         (map (lambda (a) (map (lambda (b) (make-node a b)) lisb)) lisa)))

(check-expect (whole-tree (list 'leaf) (list 'leaf))
              (list (make-node 'leaf 'leaf)))
(check-expect (whole-tree (list (make-node 'leaf 'leaf))
                          (list (make-node (make-node 'leaf 'leaf) 'leaf)
                                (make-node 'leaf 'leaf)))
              (list (make-node (make-node 'leaf 'leaf) (make-node (make-node 'leaf 'leaf) 'leaf))
                    (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))

;; le-btn : Natural -> [List-of Tree]
;; cconsumes a natural number n,
;; and creates (a list of) all leafy binary trees of height equal or less than n
(define (le-btn n)
  (if (= n 0) (list 'leaf)
          (append (list 'leaf) 
                  (whole-tree (le-btn (- n 1)) (le-btn (- n 1))))))

(check-expect (le-btn 0) (list 'leaf))
(check-expect (le-btn 1)
              (list 'leaf (make-node 'leaf 'leaf)))
(check-expect (le-btn 2)
              (list 'leaf (make-node 'leaf 'leaf)
                    (make-node 'leaf (make-node 'leaf 'leaf))
                    (make-node (make-node 'leaf 'leaf) 'leaf)
                    (make-node (make-node 'leaf 'leaf)
                               (make-node 'leaf 'leaf))))

;; leafy-binary-n : Natural -> [List-of Tree]
;; consumes a natural number n 
;; and creates (a list of) all leafy binary trees of height n
(define (leafy-binary-n n)
   (filter (lambda (btn) (= (btn-height2 btn) n)) (le-btn n)))

(check-expect (leafy-binary-n 0) (list 'leaf))
(check-expect (leafy-binary-n 1)
              (list (make-node 'leaf 'leaf)))
(check-expect (leafy-binary-n 2)
              (list (make-node 'leaf (make-node 'leaf 'leaf))
                    (make-node (make-node 'leaf 'leaf) 'leaf)
                    (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))                                   