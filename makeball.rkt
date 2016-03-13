;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname makeball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;;A ball is (posn number number)
;;A ball can be (main (make-ball (make-posn x y) 10 1
(define-struct ball(posn speed direction))
;;draw a ball
(define (draw-ball ball)
  (place-image (circle 5 "solid" "red")
  (posn-x (ball-posn ball)) (posn-y (ball-posn ball))
  BO))
  

;; the background BO 
(define BO (empty-scene 400 400))


(define world0 (make-ball (make-posn 200 200) 10 0))

;;a direction can be one of:
;;0 --> not move
;;1 --> up
;;2 --> down
;;3 --> left
;;4 --> right

(define (move-ball ball)
  (make-ball
  (cond [(= (ball-direction ball) 1) (make-posn (posn-x (ball-posn ball)) (- (posn-y (ball-posn ball)) (ball-speed ball)))]
        [(= (ball-direction ball) 2) (make-posn (posn-x (ball-posn ball)) (+ (posn-y (ball-posn ball)) (ball-speed ball)))]
        [(= (ball-direction ball) 3) (make-posn (- (posn-x (ball-posn ball))  (ball-speed ball)) (posn-y (ball-posn ball)))]
        [(= (ball-direction ball) 4) (make-posn (+ (posn-x (ball-posn ball))  (ball-speed ball)) (posn-y (ball-posn ball)))]
        [else (make-posn (posn-x (ball-posn ball)) (posn-y (ball-posn ball)))])
  (ball-speed ball) (ball-direction ball)))
  
(define (ball-key ball k)
  (make-ball (ball-posn ball) (ball-speed ball) 
             (cond [(key=? k "up") 1]
                   [(key=? k "down") 2]
                   [(key=? k "left") 3]
                   [(key=? k "right") 4]
                   [else (ball-direction ball)])))

(define (main x)
  (big-bang x
            [to-draw draw-ball]
            [on-tick move-ball]
            [on-key ball-key]))