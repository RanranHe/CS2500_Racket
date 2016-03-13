;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;average-humidity <20% "dry"
;;average-humidity >65% "humid"
;;other average-humidity "comfortable"

(define (average-humidity t)
  (cond [(< t 0.2) "dry"]
        [(> t 0.65) "humid"]
        [else "comfortable"]))

(check-expect (average-humidity 0.1) "dry")
(check-expect (average-humidity 0.2) "comfortable")
(check-expect (average-humidity 0.9) "humid")