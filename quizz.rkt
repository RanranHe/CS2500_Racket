;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quizz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (num-even alon)
  (cond [(empty? alon) 0]
        [(ons? alon) (if (even? (first alon))
                     (+ 1 (num-evens (rest alon)))
                     (num-evens (rest alon)]))