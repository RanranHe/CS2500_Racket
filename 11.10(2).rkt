;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.10(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; qsort ; [List-of Number] -> [List-of Number]
;; sort the list in ascending order
;; how : use a pivot to divide the list ...
;; terminates because ...
(define (qsort alon)
  (cond [(empty? alon) empty]
        [else (local [(define pivot (first alon))
                      (define smalls (filter (λ (x) (<= x pivot)) alon))
                      (define bigs (filter (λ (x) (> x pivot)) alon))]
                (append (qsort smalls) equals (qsort bigs)))]))

(check-expect (qsort '(9 6 3 4 7 2 1)) '(1 2 3 4 6 7 9))

;; pow : Number Number -> Number
;; calculate m to the nth power
(define (pow m n)
  (cond [(zero? n) 1]
        [else (pow m (sub1 n))]))

(check-expect (pow 2 3) 8)
(check-expect (pow 3 4) 81)
(check-expect (pow 1 3) 1)
(check-expect (pow 0 3) 0)
(check-expect (pow 3 0) 1)

; 3*3*3*3*3*3*3*3

;; Natural Natural -> Natural
;; compute the first num raised to the power of the second
;; using binary exponentiation
;; 3^8 = (3^4)^2 = (((3^2)^2)^2) = 9^2^2 =81^2
;; 3^9 = 3*3^8 = 3*(((3^2)^2)^2) = 9^2^2 =81^2
(define (pow2 m n)
  (cond [(zero? n) 1]
        [else (if (odd? n)
                  (* m (pow2 m (- n 1)))
                  (sqr (pow2 m (/ n 2))))]))
        
(check-expect (pow2 2 3) 8)
(check-expect (pow2 3 0) 1)