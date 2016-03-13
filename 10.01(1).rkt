;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.01(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A LON is one of:
;; empty
;; (cons Number LON)

(define my-grades (cons 79 (cons 85 (cons 97 empty))))

;; count: LON -> Number
;; how many nubers in the list?
(define (count alon)
  (cond [(empty? alon) 0]
        [(cons? alon) (+ (count (rest alon)))]))
(check-expect (count my-grades) 3)

;; sum: LON -> Number
;; sum the numbers in the list
(define (sum alon)
  (cond [(empty? alon) 0]
        [(cons? alon) (+ (first alon)
                         (sum (rest alon)))]))

;; LON -> Number
;; average the numbers 
(define (average alon)
  (/ (sum alon) (count alon)))
   
;; A NELON is one of:
;; (cons Number empty)
;; (cons Number NELON)
(define nelon1 (cons 90 empty))
(define nelon2 (cons 80 (cons 90 empty)))

(define (nelon-temp anelon)
  (cond [(empty? (rest nelon)) ...(first nelon)]
        [(cons? (rest nelon) ...(first nelon)
                             ...(nelon-temp (rest nelon)))]))