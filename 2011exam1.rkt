;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2011exam1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Problem 1

;; the money-make(amount each member would make) is the sum of :
;; (/ signing-bonus 12) => 500
;; (/ (* 0.1 retail-price book-number) 12)
(define (money-make x)
  (+ 500 (/ (* 0.1 49.9 x) 12)))

(check-expect (money-make 1200) 999)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 2

#|
(circle (absval (- 3 10)) "solid" "green")
=> (circle (absval -7) "solid" "green").............arith
=> (circle (cond [(< -7 0) (- -7)]
                 [else -7]) "solid" "green")........plug
=> (circle (cond [true (- -7)]
                 [else -7]) "solid" "green")........arith
=> (circle (- -7) "solid" "green")..................conditional
=> (circle 7 "solid" "green").......................arith
=> draw circle
=> FIN
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 3

#|
constructor:
make-qwoggle

predicate:
qwoggle?

selector:
qwoggle-x
qwoggle-blazt
|#

;; A Horcrux is one of:
;; (make-qwoggle Horcrux Number)
;; String
(define-struct qwoggle (x blazt))

;; A LON is one of :
;; empty
;; (cons Number LON)

;; Horcrux -> LON
#;(define (temp k)
    (cond [(qwoggle? k) ...(temp (qwoggle-x k)) ...
                        ...(qwoggle-blazt k)...]
          [(string? k) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 4

;; a half-open is (make-half-open Number Number)
;; the smaller endpoint is included
;; the larger endpoint is excluded
(define-struct half-open (left right))

;; half-open -> ???
#;(define (half-open-temp h)
    (... (half-open-left h) 
         (half-open-right h)...))

;; a interval is one of :
;; open
;; closed
;; half-open
;; half-open is one of:
;; - closed-open
;; - open-closed

;; an open is (make-open Number Number)
;; both endpoints are exculded
(define-struct open (left right))

;; a closed is (make-closed Number Number)
;; both endpoints are included
(define-struct closed (left right))

;; a open-closed is (make-open-closed Number Number)
;; left endpoint is excluded
;; right endpoint is included
(define-struct open-closed (left right))

;; a composite is (make-composite Interval Interval)
(define-struct composite (left right))

;; contains? Number Interval -> Boolean
;; consumes a half-open interval and a number
;; and determines if the number is in the interval
(define (contains? n k)
  (cond [(open? k) (if (and (> n (open-left k)) (< n (open-right k)))
                       true
                       false)]
        [(closed? k) (if (and (>= n (closed-left k)) (<= n (closed-right k)))
                         true
                         false)]
        [(half-open? k) (if (and (>= n (half-open-left k))
                                 (< n (half-open-right k)))
                            true
                            false)]
        [(open-closed? k) (if (and (> n (open-closed-left k))
                                   (<= n (open-closed-right k)))
                              true
                              false)]
        [else (if (or (contains? n (composite-left k))
                      (contains? n (composite-right k)))
                  true
                  false)]))

(check-expect (contains? 2 (make-open 0 3)) true)
(check-expect (contains? 0 (make-open 0 3)) false)
(check-expect (contains? 4 (make-half-open 0 3)) false)
(check-expect (contains? 3 (make-open-closed 0 3)) true)
(check-expect (contains? 0 (make-half-open 0 3)) true)
(check-expect (contains? 0 (make-open-closed 0 3)) false)
(check-expect (contains? 3 (make-closed 0 3)) true)
(check-expect (contains? 4 (make-composite (make-closed 0 3) (make-half-open 5 7))) false)
(check-expect (contains? 6 (make-composite (make-closed 0 3) (make-half-open 5 7))) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 5

;;; A Position is a (make-position String Number Number)
(define-struct position (name numshares share-price))
;;; Interpretation: a Position gives the name of a stock, the
;;; number of shares we own, and the current price of one share
;;; of the stock.

;;; A Portfolio is one of
;;; - empty
;;; - (cons Position Portfolio)
;;; Interpretation: a Portfolio is a collection of stock positions.

;; value: Position -> Number
;; compute the position value
(define (value po)
  (* (position-numshares po) (position-share-price po)))

;; portfolio-worth: portfolio -> Number
;; compute the worth of complete portfolio
(define (portfolio-worth p)
  (cond [(empty? p) 0]
        [(cons? p) (+ (value (first p)) (portfolio-worth (rest p)))]))

(define p1
  (cons (make-position "IBM" 10 190)
        (cons (make-position "AAPL" 5 422)
              empty)))

(check-expect (portfolio-worth p1) 4010)

