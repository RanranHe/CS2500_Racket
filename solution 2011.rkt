;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |solution 2011|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;PROBLEM 2
(require 2htdp/image)
(define (absval n)
  (cond [(< n 0) (- n)]
        [else n]))

(circle (absval (- 3 10)) "solid" "green")
;;ARITHMETIC
(circle (absval -7) "solid" "green")
;;PLUG
(circle (cond [(< -7 0) (- -7)]
              [else -7]) "solid" "green")
;;ARITHMETIC
(circle (cond [true (- -7)]
              [else -7]) "solid" "green")
;;CONDITIONAL
(circle (- -7) "solid" "green")
;;ARITHMETIC
(circle 7 "solid" "green")

;;PROBLEM 3
;; A Horcrux is one of:
;; - (make-qwoggle Horcrux Number)
;; - String
(define-struct qwoggle (x blazt))

;; Horcrux with a capital H is a data type
;; horcrux with a lower case h is a variable

;; Horcrux -> LON
(define (horcrux-to-lon-temp h)
  (cond [(string? h) ...]
        [(qwoggle? h) (... (horcrux-to-lon-temp (qwoggle-x h)) ... (qwoggle-blazt h) ...)]))

;; A LON is one of:
;; - empty
;; - (cons Number LON)

;; Constructor
;; Horcrux Number -> Horcrux
#;(make-qwoggle ... ...)
;; Predicate
;; Anything -> Boolean
#;(qwoggle? ...)
;; Selector
;; Qwoggle -> Horcrux
#;(qwoggle-x ...)
;; Qwoggle -> Number
#;(qwoggle-blazt ...)

;; PROBLEM 4

;;An Endpoint is a (make-endpt Boundary Number)
(define-struct endpt (boundary number))

;;EXAMPLES
(define INCL3 (make-endpt "inclusive" 3))
(define EXCL7 (make-endpt "exclusive" 7))
(define EXCL9 (make-endpt "exclusive" 9))
(define INCL11 (make-endpt "inclusive" 11))

;;An Interval is one of:
;; - (make-interval Endpoint Endpoint)
;; - CompoundInterval
(define-struct interval (low high))


;;A CompoundInterval is a (make-ci Interval Interval)
(define-struct ci (first second))

;;EXAMPLES
(define INT3TO7 (make-interval INCL3 EXCL7))
(define INT9TO11 (make-interval EXCL9 INCL11))
(define CI1 (make-ci INT3TO7 INT9TO11))


;;A Boundary is a String and is one of:
;; - "inclusive"
;; - "exclusive"

;;interval-template : 
#;(define (interval-template int)
  (cond [(interval? int)
         ... (endpt-template (interval-low int)) ...
         ... (endpt-template (interval-high int)) ...]
        [(ci? int)
         ... (interval-template (ci-first int))
         ... (interval-template (ci-second int))]))

;;endpt-template : Endpoint -> ???
(define (endpt-template e)
  (... (cond [(string=? (endpt-boundary e) "inclusive")
              ... (endpt-boundary e)
              ... (endpt-number e)]
             [(string=? (endpt-boundary e) "exclusive")
              ... (endpt-boundary e)
              ... (endpt-number e)]) ...))

;;contains? : Interval Number -> Boolean
;;Finds out if the number is in the interval
(check-expect (contains? INT3TO7 10) false)
(check-expect (contains? INT3TO7 4) true)
(check-expect (contains? INT3TO7 3) true)
(check-expect (contains? INT3TO7 7) false)
(define (contains? int num)
  (and (check-low (interval-low int) num)
       (check-high (interval-high int) num)))

;;check-low : Endpoint Number -> Boolean
;;Determines if a number satisfies the lower boundary
(check-expect (check-low INCL3 4) true)
(check-expect (check-low INCL3 3) true)
(check-expect (check-low EXCL7 7) false)
(define (check-low ep n)
  (cond [(string=? (endpt-boundary ep) "inclusive")
         (>= n (endpt-number ep))]
        [(string=? (endpt-boundary ep) "exclusive")
         (> n (endpt-number ep))]))

;;check-high : Endpoint Number -> Boolean
;;Determines if a number satisfies the upper boundary
(check-expect (check-high INCL3 4) false)
(check-expect (check-high INCL3 3) true)
(check-expect (check-high EXCL7 7) false)
(check-expect (check-high EXCL7 5) true)
(define (check-high ep n)
  (cond [(string=? (endpt-boundary ep) "inclusive")
         (<= n (endpt-number ep))]
        [(string=? (endpt-boundary ep) "exclusive")
         (< n (endpt-number ep))]))

;; A Comment is a (make-comment String String)
(define-struct comment (name text))
;; A LOC (list of comments) is one of:
;; - empty
;; - (cons Comment LOC)

;; Example
(define loc1 (cons (make-comment "Matthias" "You all failed")
                   (cons (make-comment "Amal"
                                       "But they tried")
                         (cons (make-comment "Matthias" "HAHAHAHAHA")
                               empty))))

;; Comment String -> Boolean
;; Is the commenter's name this string?
(define (commenter? comment name)
  (string=? (comment-name comment) name))

#|Design a program, defriend-comments, that consumes a string naming a former
friend, and a list of comments, and produces the input list without any comments
made by the former friend.|#

;; defriend-comments : String LOC -> LOC
;; removes any comments made by the former friend.
(check-expect (defriend-comments "Matthias" loc1)
              (list (make-comment "Amal"
                                  "But they tried")))
(define (defriend-comments s loc)
  (cond [(empty? loc) empty]
        [(cons? loc)
         (if (commenter? (first loc) s)
             (defriend-comments s (rest loc))
             (cons (first loc) (defriend-comments s (rest loc))))]))
(define (defriend-commentsAWESOME s loc)
  (local ((define (f comment)
            (commenter? comment s)))
  (filter f loc))) 