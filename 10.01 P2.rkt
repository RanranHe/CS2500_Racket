;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.01 P2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
;; Shape is one of:
;; -- Circle
;; -- Square
;; -- Rectangle
 
;; A Circle is a (make-circl Number Number Number Boolean Symbol)
(define-struct circl (x y r outline c))
;; interpretation: x and y determine the center of the circle,
;; r the radius, outline whether it's outlined or solid,
;; and c its color

;; A Square is a (make-squar Number Number Number Boolean Symbol)
(define-struct squar (x y size outline c))
;; interpretation: x and y determine the center of the square,
;; size the side square, outline whether it's outlined or solid,
;; and c its color
 
;; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
(define-struct recta (x y width height outline c))
;; interpretation: x and y determine the center of the rectangle,
;; width and height the length and width of rectangle, outline whether it's outlined or solid,
;; and c its color

(define sh (make-squar 100 100 50 true 'red))

;; shape-shift-x is the shame shape as sh but 
;; delta pixels shift along x-axis
(define (shape-shift-x delta) 
  (make-squar (+ (squar-x sh) delta) (squar-y sh)
              (squar-size sh) (squar-outline sh) (squar-c sh)))

(check-expect (shape-shift-x 10)
              (make-squar 110 100 50 true 'red))

;; sh is one of shapes:
;; circl
;; squar
;; recta
;; a pt is a posn
;; sh pt -> Boolean
;; pt is inside(or on the boundary) of sh?
(define pt (make-posn  130 130))
(define (shape-in? sh pt)
  (cond [(squar? sh)
         (cond [(and (and (<= (posn-x pt) (+ (squar-x sh) (/ (squar-size sh) 2)))
                          (>= (posn-x pt) (- (squar-x sh) (/ (squar-size sh) 2))))
                     (and (<= (posn-y pt) (+ (squar-y sh) (/ (squar-size sh) 2)))
                          (>= (posn-y pt) (- (squar-y sh) (/ (squar-size sh) 2)))))
                "inside"]
        [else "outside"])]
        [(circl? sh)
         (cond [(and (<= (sqrt (+ (sqr (- (posn-x pt) (circl-x sh)))
                                  (sqr (- (posn-y pt) (circl-y sh)))))
                         (circl-r sh))
                     (>= (sqrt (+ (sqr (- (posn-x pt) (circl-x sh)))
                                  (sqr (- (posn-y pt) (circl-y sh)))))
                         0))
                "inside"]
               [else "outside"])]
        [(recta? sh)
         (cond [(and (and (<= (posn-x pt) (+ (recta-x sh) (/ (recta-width sh) 2)))
                          (>= (posn-x pt) (- (recta-x sh) (/ (recta-width sh) 2))))
                     (and (<= (posn-y pt) (+ (recta-y sh) (/ (recta-height sh) 2)))
                          (>= (posn-y pt) (- (recta-y sh) (/ (recta-height sh) 2)))))
                "inside"]
        [else "outside"])]))
         
(check-expect (shape-in? sh pt) "outside")
(check-expect (shape-in? sh (make-posn 100 80)) "inside")
(check-expect (shape-in? (make-circl 100 100 10 true 'red) pt) "outside")
(check-expect (shape-in? (make-circl 100 100 60 true 'red) (make-posn 60 60)) "inside")
(check-expect (shape-in? (make-recta 50 50 30 50 true 'red) pt) "outside")
(check-expect (shape-in? (make-recta 150 200 130 100 true 'red) (make-posn 110 150)) "inside")

              
;; sc is the empty-scene 300 300
;; sh is one of the shapes:
;; circl
;; squar
;; recta
;; adds sh to sc
(define sc (empty-scene 300 300))
(define (shape-draw sh)
  (cond 
    [(circl? sh) 
     (overlay (circle (circl-r sh) 
                      (cond [(boolean=? #t (circl-outline sh)) "outline"]
                            [(boolean=? #f (circl-outline sh)) "solid"])
                      (circl-c sh)) sc)]
    [(squar? sh) 
     (overlay (square (squar-size sh)
                      (cond [(boolean=? #t (squar-outline sh)) "outline"]
                            [(boolean=? #f (squar-outline sh)) "solid"])
                      (squar-c sh)) sc)]
    [(recta? sh) 
     (overlay (rectangle (recta-width sh)
                         (recta-height sh)
                         (cond [(boolean=? #t (recta-outline sh)) "outline"]
                               [(boolean=? #f (recta-outline sh)) "solid"])
                         (recta-c sh)) sc)]))
(check-expect (shape-draw (make-circl 100 100 10 true 'red)) (overlay (circle 10 "outline" 'red) sc))
(check-expect (shape-draw (make-squar 100 100 20 true 'pink)) (overlay (square 20 "outline" 'pink) sc))
(check-expect (shape-draw (make-recta 100 100 300 100 true 'green)) (overlay (rectangle 300 100 "outline" 'green) sc))
(check-expect (shape-draw (make-circl 100 100 10 false 'red)) (overlay (circle 10 "solid" 'red) sc))
(check-expect (shape-draw (make-squar 100 100 20 false 'pink)) (overlay (square 20 "solid" 'pink) sc))
(check-expect (shape-draw (make-recta 100 100 300 100 false 'green)) (overlay (rectangle 300 100 "solid" 'green) sc))
        