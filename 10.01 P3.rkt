;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.01 P3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; the length of password is between 6 and 10? (include 6 10)
(define (check-pass-6-10? pas)
  (cond [(empty? pas) false]
        [(cons? pas) (cond [(and (>= (length pas) 6) (<= (length pas) 10))
                            true]
                           [else false])]))

(define list1 (cons "h" (cons "e" (cons "r" (cons "e" (cons 0 (cons 9 (cons 9 empty))))))))
(define list2 (cons "e" (cons"d" (cons 0 empty))))

(check-expect (check-pass-6-10? empty) false)
(check-expect (check-pass-6-10? list1) true)
(check-expect (check-pass-6-10? list2) false)

;; the length of password is between min and max? (include min and max)
(define (check-pass? pas min max)
  (cond [(empty? pas) false]
        [(cons? pas) (cond [(and (>= (length pas) min) (<= (length pas) max))
                            true]
                           [else false])]))

(check-expect (check-pass? empty 10 11) false)
(check-expect (check-pass? list1 3 7) true)
(check-expect (check-pass? list2 10 11) false)


