;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.25(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (add-up c)
  (cond [(string? c) 0]
        [(combine? c) (+ (combine-one c) ;; Number
                        (add-up (combine-two c)))]))

(check-expect (add-up (make-combine 1 (make-combine 2 "done"))) 3)
(check-expect (add-up c2) 0)

;;combination -> Boolean
;; is zero in the combination?
(define (zero-in? c)
  (cond [(string? c) false]
        [(combine? c) (cond [(zero? (combine-one c) true
                      ...(zero-in? (combine-two c))]))
  

(check-expect (zero-in? c2) falses)
(check-expect (zero-in? c4) true)