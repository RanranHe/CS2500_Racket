;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.18(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct ship (name speed weight))
;; A ship has a name, a speed (in knota) and a weight (in tons)
;; Example ship:
(define ship1 (make-ship "Titanic" 30 1000))

;; Design a program to "load a ship" with
;; A given weight of cargo

;; Example: (load-ship ship1 500) --> (make-ship "Titanic" 30 1500)

;; Ship number -> ship
;; load the given ship with the weight of the given cargo
(define (load-ship sh cargo)
  (make-ship (ship-name sh)
             (ship-speed sh)
             (+ cargo (ship-weight sh))))

(check-expect (load-ship ship1 500)
              (make-ship "Titanic" 30 1500))