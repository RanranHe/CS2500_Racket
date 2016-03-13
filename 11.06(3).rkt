;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.06(3)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A BST is one of :
;; (make-leaf)
;; (make-branch Narural String BST BST)
;; (make-branch n key tl tr)
;; Constraint : all numbers in tr
;; are larger than n
;; this is also true for every subtree

;; Natural String BST -> BST
;; add the given info and key to the given tree
(define (grow k i abst)
  (cond [(leaf? abst) (make-branch k i(make-leaf) (make-leaf))]
        [(branch? abst) 
         (cond [(= k (branch-key abst)) (make-branch k i (branch-left abst) (branch-right abst))]
               [(< k (branch-key abst)) (make-branch (branch-key abst) (branch-info abst)
                                                     (grow k i (branch-left abst))
                                                     (branch-right abst))]
               [else (make-branch (branch-key abst) (branch-info abst)
                                  (branch-left abst)
                                  (grow k i (branch-right abst)))])]))
               
                             

(check-expect (grow 11 "f" bst3)
              (make-branch 10 "a"
                           (make-branch 8 "b"
                                        (make-branch 7 "c" bst1 bst1) bst1)
                           (make-branch 12 "d" bst1
                                        (make-branch 14 "e"
                                                     (make-branch 13 "f" bst1 bst1)
                                                     bst1))))
(check-expect (grow 5 "f" bst3)
              (make-branch 10 "a"
                           (make-branch 8 "b"
                                        (make-branch 7 "c" bst1 bst1) bst1)
                           (make-branch 12 "d" bst1
                                        (make-branch 14 "e"
                                                     (make-branch 13 "f" bst1 bst1)
                                                     bst1))))



;; Natural BST -> String
;; retrieve the info that comes with the given k
(define (retrieve key abst)
  (cond [(leaf? abst) (error "not found")]
        [(branch? abst) (cond [(= key (branch-key abst)) (branch-info abst)]
                              [(< key (branch-key abst)) (retrieve key (branch-left abst))]
                              [else (retrieve key (branch-right abst))])]))

(check-expect (retrieve 12 bst3) "d")
(check-expect (retrieve 12 bst1) "not found")
 
 
 
 
(define-struct leaf ())
(define-struct branch (key info left right))
(define bst1 (make-leaf))
(define bst2 (make-branch 0 "a" (make-leaf) (make-leaf)))
(define bst3 (make-branch 10 "a"
                         (make-branch 8 "b"
                                      (make-branch 7 "c" bst1 bst1) bst1)
                         (make-branch 12 "d" bst1
                                      (make-branch 14 "e"
                                                  (make-branch 13 "f" bst1 bst1)
                                                  bst1))))


