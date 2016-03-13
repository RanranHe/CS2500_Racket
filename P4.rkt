;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define red-dot (circle 5 "solid" "red"))
(define blue-dot (circle 5 "solid" "blue"))
(define green-dot (circle 5 "solid" "green"))
(define BO (empty-scene 200 200))

;;black-line over ...
;;green-dot over ...
;;red-dot (x1 y1) over ...
;;blue-dot (x2 y2)over BO 
(define (table x1 y1 x2 y2) 
  (add-line (place-image green-dot 
                         (+ (* (/ (- x2 x1) 2) 0.45) x1)
                         (+ (* (/ (- y2 y1) 2) 0.45) y1)
                         (place-image red-dot x1 y1
                                      (place-image blue-dot x2 y2 BO)))
            x1 y1 x2 y2 "black"))

(check-expect (table 10 12 160 180)
             (add-line (place-image green-dot 43.75 49.8
                         (place-image red-dot 10 12
                                      (place-image blue-dot 160 180 BO)))
            10 12 160 180 "black"))

             
               
                                                      