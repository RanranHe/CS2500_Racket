;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 14exam1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Problem 1

;; A aver-am is the amount each diner should pay:
;; (/ (* b (+ 1 t)) d)
;; b is dinner bill
;; d is dinner number
;; t is the tip(fraction)
(define (aver-am b d t)
  (/ (* b (+ 1 t)) d))

(check-expect (aver-am 200 10 0.15) 23)

; Problem 2

#| 
(* 3 2) => 6.......................arith
(cond [(> 6 5) (sqr (- 6 2))]
      [else (+ 6 3)])).............plug
(cond [true (sqr (- 6 2))]
      [else (+ 6 3)])).............conditional
(sqr (- 6 2))......................conditional
(sqr 4)............................arith
16.................................arith
|#

; Problem 3

(define-struct frobboz (a b))
(define-struct frabble (b c))
;; A Foo is one of:
;; a symbol
;; (make-frobboz Foo Number)
;; (make-frabble Boolean Foo)
(define (temp foo)
  (cond [(symbol? foo) ...]
        [(frobboz? foo) ...]
        [(frabble? foo) ...]))

; Problem 4

(define-struct donation (donor amount))
;; A Donation is one of:
;; Number
;; (make-donation String Number)
;; where a simple number represents an anonymous donation

;; donation => Boolean
;; Is a donation illegal?
(define (bad-donation? don)
  (cond [(number? don) (if (<= don 50) false true)]
        [(donation? don) (if (= (donation-amount don) 2500) false true)]))

(check-expect (bad-donation? 100) true)
(check-expect (bad-donation? 10) false)
(check-expect (bad-donation? (make-donation "John" 1000)) true)
(check-expect (bad-donation? (make-donation "Mary" 2500)) false)

;; LOD => Boolean
;; A LOD(list of donations) is one of:
;; empty
;; (cons Donation LOD)
;; are any of the donations on the list illegal ones?
(define (any-bad-donations? lod)
  (cond [(empty? lod) false]
        [(cons? lod) (if (bad-donation? (first lod)) true
                         (any-bad-donations? (rest lod)))]))
(check-expect (any-bad-donations? empty) false)
(check-expect (any-bad-donations? 
               (cons (make-donation "Mary" 2500) (cons 25 (cons 10 empty))))
              false)
(check-expect (any-bad-donations? 
               (cons (make-donation "Jerry" 1500) (cons 25 (cons 55 empty))))
              true)

  
;; Problem 5

;; LOD => Number(total amount donated by same person)
;; A LOD(list of donations) is one of:
;; empty
;; (cons Donation LOD)
;; compute total amount donated by same person
(define (donor-total name lod)
  (cond [(empty? lod) 0] 
        [(cons? lod) 
         (cond [(donation? (first lod))
                (if (string=? (donation-donor (first lod)) name)
                    (+　(donation-amount (first lod)) (donor-total name (rest lod)))
                    (donor-total name (rest lod)))]
               [(number? (first lod)) (donor-total name (rest lod))])]))

(check-expect (donor-total "Mary" (cons (make-donation "Mary" 2500)
                                 (cons 10 (cons (make-donation "Jerry" 2500)
                                                (cons (make-donation "Mary" 2500)
                                                                     (cons 70 empty))))))
              5000)
(check-expect (donor-total "Jerry" empty) 0)
 
; Problem 6

;; A coment is a (make-comment String String)
(define-struct comment (name text))

;; A LOC (list of comments) is one of:
;; empty
;; (cons comment LOC)
(define loc1 (cons (make-comment "Amy" "How are you?")
                   (cons (make-comment "Alice" "Good for you!")
                         (cons (make-comment "John" "So far, so good?")
                               (cons (make-comment "Jam" "not very good")
                                     (cons (make-comment "Alice" "Great!") empty))))))

;; list -> list
;; delete the former friend's comments on the list
(define (defriend-comments name loc)
  (cond [(empty? loc) empty]
        [(cons? loc) 
         (if (string=? name (comment-name (first loc)))
             (defriend-comments name (rest loc))
             (cons (first loc) (defriend-comments name (rest loc))))]))

(check-expect (defriend-comments "Alice" loc1)
              (cons (make-comment "Amy" "How are you?")
                    (cons (make-comment "John" "So far, so good?")
                          (cons (make-comment "Jam" "not very good") empty))))
(check-expect (defriend-comments "Mary" empty) empty)
                              
              
                      


