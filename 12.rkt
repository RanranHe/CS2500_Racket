;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct time (hours minutes))


(define (tock x)
  (cond [(< (time-minutes x) 59) (make-time (time-hours x) (+ (time-minutes x) 1))]
        [(and (= (time-minutes x) 59) (< (time-hours x) 11)) (make-time (+ (time-hours x) 1) (- (time-minutes x) 59))]
        [(and (= (time-minutes x) 59) (= (time-hours x) 11)) (make-time (- (time-hours x) 11) (- (time-minutes x) 59))]))

(check-expect (tock (make-time 11 59)) (make-time 0 0))

(define BO (rectangle 80 40 "outline" "black")) 



(define (time->text x)
  (place-image (text (number->string (time-hours x)) 11 "red") 20 20
               (place-image (text ":" 11 "black") 40 20
                            (place-image (text (number->string (time-minutes x)) 11 "blue") 60 20 BO))))

(define (main x)
  (big-bang x
            [to-draw time->text]
            [on-tick tock]))
            

  
  