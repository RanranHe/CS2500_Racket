;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.01 P1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; regular is a (make-regular String Number Number)
(define-struct regular (product-id base-price year))

;; classic is a (make-classic String Number)
(define-struct classic (product-id price))

(define regular1 (make-regular "234-87-1DX" 234 1987))

;; Number -> Number
;; compute the current-price as:
;; base-price - 0.035 * base-price * year (current-price>=2)
;; 2 (current-price<2)
(define (movie x)
  (cond [(regular? x)
         (cond [(>= (- (regular-base-price x) (* (* 0.035 (regular-base-price x)) (- 2014 (regular-year x)))) 2)
                (- (regular-base-price x) (* (* 0.035 (regular-base-price x)) (- 2014 (regular-year x))))]
               [(< (- (regular-base-price x) (* (* 0.035 (regular-base-price x)) (- 2014 (regular-year x)))) 2) 2])]
        [(classic? x) (classic-price x)]))

(check-expect (movie (make-regular "100-04-1SX" 100 2004)) 65)
(check-expect (movie (make-classic "50-ih8" 50)) 50)

