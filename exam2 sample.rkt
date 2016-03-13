;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |exam2 sample|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;==============================2012fall-exmam2==================================;;

;;================Problem 1================;;
;; Suppose we have the two lists
(define a '(1 2))
(define b '((3 4) (5 6)))
;; What do each of the following expressions produce?
; 1. (append a b) => (list 1 2 (list 3 4) (list 5 6))
; 2. (list a b) => (list (list 1 2) (list (list 3 4) (list 5 6)))
; 3. (cons a b) => (list (list 1 2) (list 3 4) (list 5 6))
; 4. (apply append a b) => (list 1 2 3 4 5 6)

;;================Problem 2================;; 
;; Suppose we use the following data definition to represent binary trees
;; with numbers at the leaves:

;;; A BT is one of:
;;; - Number
;;; - (make-node BT BT)
(define-struct node (left right))

;; Here are two example binary trees:
(define tree1 (make-node (make-node 10 9)
                         (make-node 3
                                    (make-node 1 5))))
(define tree2 7)
(define tree0 (make-node (make-node 10 9)
                         (make-node (make-node (make-node (make-node (make-node 8 7) (make-node 6 5)) 2)
                                               (make-node 6 5))
                                    (make-node (make-node (make-node (make-node 8 7) (make-node 6 5)) 2)
                                               (make-node 6 5)))))
;; Design a function to flatten a tree into the list of its elements, read left-to-right.
;; So, tree1 should flatten out to the list
;; (list 10 9 3 1 5)

#;(define (temp t)
    (cond [(number? t) ...]
          [(node? t) ...(temp (node-left t))
                     ...(temp (node-right t))]))

;; flatten-tree : Tree -> [List-of Number]
;; flatten a tree into the list of its elements, read left-to-right
(define (flatten-tree t)
    (cond [(number? t) (cons t empty)]
          [(node? t) (append (flatten-tree (node-left t)) (flatten-tree (node-right t)))]))

(check-expect (flatten-tree tree1) (list 10 9 3 1 5))
(check-expect (flatten-tree tree2) (list 7))
(check-expect (flatten-tree tree0) (list 10 9 8 7 6 5 2 6 5 8 7 6 5 2 6 5))

;;================Problem 3================;;
;; Design a function, same-shape? to determine if two trees have the same shape.
(define tree3 (make-node (make-node 9 10)
                         (make-node 5
                                    (make-node 0 -7))))
(define tree0-1 (make-node (make-node 9 9)
                         (make-node (make-node (make-node (make-node (make-node 8 7) (make-node 6 5)) 2)
                                               (make-node 6 5))
                                    (make-node (make-node (make-node (make-node 8 7) (make-node 3 5)) 2)
                                               (make-node 4 5)))))
(define tree0-2 (make-node (make-node 9 9)
                         (make-node (make-node (make-node (make-node (make-node 8 7) (make-node 6 5)) 2)
                                               (make-node 6 5))
                                    (make-node (make-node (make-node (make-node 8 7) (make-node 3 5)) 2)
                                               (make-node (make-node (make-node (make-node 8 7) 7) 
                                                                     (make-node 3 5)) 5)))))
(define tree0-3 (make-node (make-node 9 9)
                         (make-node (make-node (make-node (make-node (make-node 8 7) (make-node 6 5)) 2)
                                               (make-node 6 5))
                                    (make-node (make-node (make-node (make-node 8 7) (make-node 3 5)) 2)
                                               (make-node (make-node (make-node 9 7) 
                                                                     (make-node 3 5)) 5)))))
;; Design a function, same-shape? to determine if two trees have the same shape.

;; same-shape? : Tree Tree -> Boolean
;; determine if two trees have the same shape
(define (same-shape? t1 t2)
  (or (and (number? t1) (number? t2))
      (and (node? t1) (node? t2)
           (same-shape? (node-left t1) (node-left t2))
           (same-shape? (node-right t1) (node-right t2)))))
#;(cond [(and (number? t1) (number? t2)) true]
        [(or (and (number? t1) (node? t2))
             (and (node? t1) (number? t2))) false]
        [else (if (same-shape? (node-left t1) (node-left t2))
                  (same-shape? (node-right t1) (node-right t2))
                  false)])

(check-expect (same-shape? tree1 tree3) true)
(check-expect (same-shape? tree0 tree0-1) true)
(check-expect (same-shape? tree0 tree0-2) false)
(check-expect (same-shape? tree2 tree3) false)
(check-expect (same-shape? tree0-2 tree0-3) false)

;;================Problem 4================;;
;; We can represent songs in a music collection with the following data definition:
;; A Song is a (make-song String String Number)
(define-struct song (title artist length))
;; where the length of the song is given in seconds

(define song1 (make-song "Hey, Jude" "The beatles" 431))
(define songs (list song1
                    (make-song "U Smile" "Justin Bieber" 197)
                    (make-song "Free Bird" "Lynyrd Skynyrd" 608)))

;;=====Question 1=====;;
;; Design a function, total-time, that consumes a list of songs and produces
;; the total length of all the songs. Write your function using a loop function
;; but do not use apply.

;; total-time : [List-of Songs] -> Number
;; consumes a list of songs and produces the total length of all the songs
(define (total-time los)
  (foldr (λ (s n) (+ (song-length s) n)) 0 los))
#;(foldr + 0 (map song-length los))

(check-expect (total-time empty) 0)
(check-expect (total-time songs) 1236)
(check-expect (total-time (list song1)) 431)

;;=====Question 2=====;;
;; Design a function, any-bieber? that returns true if the input list has one or
;; more songs by artist "Justin Bieber"
;; Again, use a loop function; again, don’t use apply.

;; any-bieber? : [List-of Songs] -> Boolean
;; returns true if the input list has one or more songs by artist "Justin Bieber"
(define (any-bieber? los)
  (ormap (λ (s) (string=? (song-artist s) "Justin Bieber")) los)) 

(check-expect (any-bieber? songs) true)
(check-expect (any-bieber? empty) false)
(check-expect (any-bieber? (list song1)) false)

;;=====Question 3=====;;
;; Design a function, remove-artist, that takes a list of songs, and a string
;; giving the name of an artist, and strips all the songs from the input list by
;; that artist. As usual, use a loop function, and no apply.

;; remove-artist : [List-of Songs] String -> [List-of Songs]
;; takes a list of songs, and a string giving the name of an artist,
;; and strips all the songs from the input list by that artist
(define (remove-artist los n)
  (local [;; same-name? : Song -> Boolean
          ;; determine if the artist of song is the same as the given name
          ;; if same, give false
          (define (same-name? s) (not (string=? (song-artist s) n)))]
    (filter same-name? los)))
 
(check-expect (remove-artist songs "The beatles")
              (list (make-song "U Smile" "Justin Bieber" 197)
                    (make-song "Free Bird" "Lynyrd Skynyrd" 608)))
(check-expect (remove-artist (list song1) "Lynyrd Skynyrd") 
              (list (make-song "Hey, Jude" "The beatles" 431)))

;;=====Question 4=====;;
;; Give a contract and purpose statement for the following function.

;; quicksort : [List-of X] [X X -> Boolean] -> [List-of X]

;; fred : [List-of Songs] -> [List-of Songs]
;; give the ranking of songs depend on the length of songs, from shortest to longest
(define (fred xs)
  (quicksort xs
             (lambda (a b) (< (song-length a) ;; [Song Song -> Boolean]
                              (song-length b)))))

(check-expect (fred (cons (make-song "Hey, Jude" "The beatles" 700) 
                          (cons (make-song "Hey, Jude" "The beatles" 200) songs)))
              (list (make-song "U Smile" "Justin Bieber" 197)
                    (make-song "Hey, Jude" "The beatles" 200) 
                    (make-song "Hey, Jude" "The beatles" 431)
                    (make-song "Free Bird" "Lynyrd Skynyrd" 608)
                    (make-song "Hey, Jude" "The beatles" 700) ))

;;================Problem 5================;;
;; A nodee is (make-nodee GradeTree Number GradeTree)
(define-struct nodee (left val right))
;;; A GradeTree is one of:
;;; - a Grade
;;; - (make-nodee GradeTree Grade GradeTree)

;;; A Grade is a number in the range [0,100].
;; Here are three example GradeTrees:
(define gradetree1 (make-nodee (make-nodee 3 0 1)
                         10
                         (make-nodee 8 100 (make-nodee 1 2 3))))
(define gradetree2 (make-nodee (make-nodee 1 0 10)
                         3
                         105))
(define gradetree3 (make-nodee (make-nodee 1 5 8)
                         10
                         (make-nodee (make-nodee 11 17 18)
                                    23
                                    87)))
(define gradetree4 (make-nodee 3
                         10
                         (make-nodee 20 25 (make-nodee 40 50 60))))
(define gradetree5 (make-nodee 
                    (make-nodee 3 9 3)
                    10
                    (make-nodee 20 25 30)))
;; A GradeTree nodee is ordered if
;; • all the numbers in its left child are smaller than the nodee’s value,
;; • all the numbers in its right child are greater than the nodee’s value, and
;; • the left and right subtrees are both ordered

;; Please design a function tree-ordered? that takes a GradeTree 
;; and determines of the tree is ordered or not.

;; gradetree-ordered? : GradeTree -> Boolean
;; takes a GradeTree and determines of the gradetree is ordered or not.
(define (tree-ordered? gt)
  (cond [(number? gt) true]
        [(nodee? gt) (if (and (andmap (λ (x) (< x (nodee-val gt)))
                                          (flatten-gradetree (nodee-left gt)))
                              (andmap (λ (x) (> x (nodee-val gt)))
                                          (flatten-gradetree (nodee-right gt))))
                         (if (tree-ordered? (nodee-left gt))
                             (tree-ordered? (nodee-right gt))
                             false)
                         false)]))

(check-expect (tree-ordered? gradetree5) false)
(check-expect (tree-ordered? gradetree1) false)
(check-expect (tree-ordered? gradetree2) false)
(check-expect (tree-ordered? gradetree3) true)
(check-expect (tree-ordered? gradetree4) true)
(define ordered-example (make-nodee 10 50 80))
(check-expect (tree-ordered? ordered-example) true)
(define unordered-example (make-nodee (make-nodee 12 30 51) 50 80))
(check-expect (tree-ordered? unordered-example) false)

;; flatten-gradetree : GradeTree -> [List-of Number]
;; flatten a GradeTree into the list of its elements, read left-to-right
(define (flatten-gradetree gt)
    (cond [(number? gt) (cons gt empty)]
          [(nodee? gt) (append (flatten-gradetree (nodee-left gt))
                               (flatten-gradetree (nodee-val gt))
                               (flatten-gradetree (nodee-right gt)))]))

(check-expect (flatten-gradetree 3) (list 3))
(check-expect (flatten-gradetree gradetree1) (list 3 0 1 10 8 100 1 2 3))