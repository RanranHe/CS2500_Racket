;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;;put the pentagon over the square
;;pentagon's side-length: length1
;;square's length: length2

(define (favorite-star length1 length2 color)
  (place-image (regular-polygon length1 5 "solid" color)
               (/ length2 2) (/ length2 2)
               (square length2 "outline" color)))

(check-expect (favorite-star 10 40 "red")
  (place-image (regular-polygon 10 5 "solid" "red")
               20 20
               (square 40 "outline" "red")))