;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define-struct STAR (L 5 "solid" C))
(define-struct SQUARE (L "solid" C))

(define favorite-star (place-image STAR
                                   (/ L 2) (/ L 2)
                                   SQUARE))


  