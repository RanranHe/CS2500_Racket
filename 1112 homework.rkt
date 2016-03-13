;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1112 homework|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;=============Problem 1==============;;
;;; An Atom is one of:
;;; - Number
;;; - Symbol
;;; - String
;;;
;;; An SExp is one of:
;;; - Atom
;;; - [List-of SExp]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;===========================LEGO==============================;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct lego (label color width))
; A Lego is a structure:
;    (make-lego Number Symbol Number)
; interpretation: (make-lego l c w) is the lego brick
; with label l, color c, and width w (in pixels).
 
(define-struct bigger (lego left right))
; A LegoBldg (lego building) is one of:
; - Lego
; - (make-bigger Lego LegoBldg LegoBldg)
; interpretation: (make-bigger l lft rgt) makes a bigger
; lego building by putting a lego brick l on top of two lego
; buildings lft (left) and rgt (right).

;;=============Problem 2==============;;
;; count-bricks : LegoBldg -> Number
;; takes a lego building and produces 
;; the total number of lego bricks in that building
(define (count-bricks lgb)
  (cond [(lego? lgb) 1]
        [(bigger? lgb) (+ 1 (count-bricks (bigger-left lgb))
                          (count-bricks (bigger-right lgb)))]))
                           
(check-expect (count-bricks 
               (make-bigger (make-lego 1 'red 3) 
                            (make-bigger (make-lego 1 'red 3)
                                         (make-lego 1 'red 3)
                                         (make-lego 1 'red 3))
                            (make-bigger (make-lego 1 'red 3)
                                         (make-bigger 
                                          (make-lego 1 'red 3)
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)))))
              15)
(check-expect (count-bricks (make-lego 1 'red 3)) 1)

;;================Problem 3================;;
;; how-high : LegoBldg -> Number
;; takes a lego building and produces 
;; the total height of the lego building (in pixels)
(define (how-high lgb)
  (cond [(lego? lgb) 10]
        [(bigger? lgb) (if (>= (+ 10 (how-high (bigger-left lgb)))
                               (+ 10 (how-high (bigger-right lgb))))
                           (+ 10 (how-high (bigger-left lgb)))
                           (+ 10 (how-high (bigger-right lgb))))]))

(check-expect (how-high 
               (make-bigger (make-lego 1 'red 3) 
                            (make-bigger (make-lego 1 'red 3)
                                         (make-lego 1 'red 3)
                                         (make-lego 1 'red 3))
                            (make-bigger (make-lego 1 'red 3)
                                         (make-bigger 
                                          (make-lego 1 'red 3)
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)))))
              50)
(check-expect (how-high (make-lego 1 'red 3)) 10)


;;================Problem 4================;;
;; contains-colored-brick? : LegoBldg Symbol -> Boolean
;; takes a lego building and a color, 
;; and determines whether the building contains a lego brick of the given color
(define (contains-colored-brick? lgb c)
  (cond [(lego? lgb) 
         (if (symbol=? (lego-color lgb) c)
             true 
             false)]
        [(bigger? lgb) 
         (or (contains-colored-brick? (bigger-lego lgb) c)
             (contains-colored-brick? (bigger-right lgb) c)
             (contains-colored-brick? (bigger-left lgb) c))]))

(check-expect (contains-colored-brick? 
               (make-bigger (make-lego 1 'red 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 1 'yellow 3)
                                         (make-lego 1 'yellow 3))
                            (make-bigger (make-lego 1 'orange 3)
                                         (make-bigger 
                                          (make-lego 1 'orange 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'blue 3))))
              'blue)
              true)
(check-expect (contains-colored-brick? 
               (make-bigger (make-lego 1 'red 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 1 'yellow 3)
                                         (make-lego 1 'orange 3))
                            (make-bigger (make-lego 1 'blue 3)
                                         (make-bigger 
                                          (make-lego 1 'orange 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'blue 3))))
              'orange)
              true)
(check-expect (contains-colored-brick? 
               (make-bigger (make-lego 1 'red 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 1 'yellow 3)
                                         (make-lego 1 'orange 3))
                            (make-bigger (make-lego 1 'blue 3)
                                         (make-bigger 
                                          (make-lego 1 'orange 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 1 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'blue 3))))
              'pink)
              false)
(check-expect (contains-colored-brick? (make-lego 1 'red 3) 'red) true)
(check-expect (contains-colored-brick? (make-lego 1 'orange 3) 'red) false)

;;================Problem 5================;;
;; find-colored-brick? : LegoBldg Symbol -> Lego
;; takes a lego building and a color 
;; and finds any lego with the given color in the building,
;; or returns false if there are no such legos
(define (find-colored-brick? lgb c)
  (cond [(lego? lgb) 
         (if (symbol=? (lego-color lgb) c)
             lgb 
             false)]
        [(bigger? lgb) 
         (if (symbol=? (lego-color (bigger-lego lgb)) c)
             (bigger-lego lgb)
             (check-lego (bigger-left lgb) (bigger-right lgb) c))]))

(define (check-lego lgb1 lgb2 c)
  (cond [(lego? lgb1) 
         (if (symbol=? (lego-color lgb1) c)
             lgb1 
             false)]
        [(lego? lgb2) 
         (if (symbol=? (lego-color lgb2) c)
             lgb2
             false)]
        [else (if (symbol=? (lego-color (bigger-lego lgb1)) c)
                  (bigger-lego lgb1)
                  (if (symbol=? (lego-color (bigger-lego lgb2)) c)
                      (bigger-lego lgb2)
                      (check-lego (bigger-left lgb) (bigger-right lgb) c)))]))

(check-expect (find-colored-brick? 
               (make-bigger (make-lego 1 'green 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-bigger 
                                          (make-lego 2 'green 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 2 'blue 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 3 'blue 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 1 'blue 3)))
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 1 'yellow 3)
                                         (make-lego 1 'orange 3)))
              'blue)
              (make-lego 2 'blue 3))
(check-expect (find-colored-brick? 
               (make-bigger (make-lego 10 'red 3)
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 1 'yellow 3)
                                         (make-lego 1 'orange 3))
                            (make-bigger (make-lego 1 'blue 3)
                                         (make-bigger 
                                          (make-lego 1 'orange 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 3 'red 3))
                                          (make-bigger (make-lego 4 'red 3)
                                                       (make-lego 5 'red 3)
                                                       (make-lego 6 'red 3)))
                                         (make-bigger (make-lego 2 'red 3)
                                                      (make-lego 7 'red 3)
                                                      (make-lego 9 'blue 3))))
              'red)
              (make-lego 10 'red 3))
(check-expect (find-colored-brick? 
               (make-bigger (make-lego 1 'green 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-bigger 
                                          (make-lego 2 'green 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 2 'pink 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 3 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 3 'orange 3)))
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 8 'blue 3)
                                         (make-lego 1 'orange 3)))
              'blue)
              (make-lego 8 'blue 3))
(check-expect (find-colored-brick? 
               (make-bigger (make-lego 1 'green 3) 
                            (make-bigger (make-lego 1 'green 3)
                                         (make-bigger 
                                          (make-lego 2 'green 3)
                                          (make-bigger (make-lego 1 'green 3)
                                                       (make-lego 2 'pink 3)
                                                       (make-lego 1 'red 3))
                                          (make-bigger (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)
                                                       (make-lego 1 'red 3)))
                                         (make-bigger (make-lego 3 'red 3)
                                                      (make-lego 1 'red 3)
                                                      (make-lego 3 'orange 3)))
                            (make-bigger (make-lego 1 'green 3)
                                         (make-lego 8 'blue 3)
                                         (make-lego 1 'orange 3)))
              'yellow)
              false)

;;================Problem 6================;;
;; lego->image : Lego -> Image
;; takes a lego and produces an image of the lego
(define (lego->image l)
  (rectangle (lego-width l) 10 'solid (lego-color l)))

(check-expect (lego->image (make-lego 1 'red 40)) 
              (rectangle 40 10 'solid 'red))

;; lb->image : LegoBldg -> Image
;; takes a lego building and produces an image of the building
(define (lb->image lgb)
  (cond [(lego? lgb) (lego->image lgb)]
        [(bigger? lgb) 
         (above (lego->image (bigger-lego lgb))
                (beside/align "top"
                              (lb->image (bigger-left lgb))
                              (lb->image (bigger-right lgb))))]))

(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-bigger (make-lego 6 'orange 60)
                                                   (make-lego 5 'green 40)
                                                   (make-lego 7 'red 40))))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "top" 
                                   (above (rectangle 60 10 'solid 'blue)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'yellow)
                                                        (rectangle 40 10 'solid 'red)))
                                   (above (rectangle 60 10 'solid 'orange)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'green)
                                                        (rectangle 40 10 'solid 'red))))))
(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-lego 6 'orange 60)))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "top" 
                                   (above (rectangle 60 10 'solid 'blue)
                                          (beside/align "top"
                                                        (rectangle 40 10 'solid 'yellow)
                                                        (rectangle 40 10 'solid 'red)))
                                   (rectangle 60 10 'solid 'orange))))