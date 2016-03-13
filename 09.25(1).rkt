;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.25(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; (cons first rest)

;;cons?

;; selectors:
;; first
;; rest

;; empty
;; empty?

empty
(define list1 (cons 1 empty))
(define list2 (cons 2 (cons 1 empty)))
(define list3 (cons 3 (cons 2 (cons 1 empty))))

;; A LON is one of 
;; empty
;; (cons Number LON)

;; LON -> nUMBER
;; add up the numbers in the list
(define (add-up alon)
  (cond [(empty? alon) 0]
        [(cons? alon) (+ (first alon)
                         (add-up (rest alon)))]))


(check-expect (add-up empty) 0)
(check-expect (add-up list1) 1)
(check-expect (add-up list2) 3)

;; A LOS is one of 
;; empty
;; (cons String LOS)

(define los1 empty)
(define los2 (cons "Hello" empty))
(define los3 (cons "World" los2))

;; A LOP is one of 
;; empty
;; (cons Posn LOP)

(define lop1 empty)
(define lop2 (cons (make-posn 10 20) empty))
(define lop3 (cons (make-posn 30 50) (cons (make-posn 60 80) lop2)))

;; LOP -> Image
;; draw the posns onto the scene
(define (draw-lop alop)
  (cond [(empty? alop) (empty-scene 150 150)]
        [(cons? alop) (draw-posn (first alop)
                                 (draw-lop (rest alop)))]))

(check-expect (draw-lop lop1) (empty-scene 150 150))
(check-expect (draw-lop lop2) (place-image (star 10 'solid 'blue)
                                           10 20
                                           (empty-scene 150 150)))

;; Posn Image -> Image
;; draw one posn onto the image
(define (draw-posn p sc)
  (place-image (star 10 'solid 'blue
