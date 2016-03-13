;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.29(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A LOS is one of 
;; empty
;; (cons String LOS)

(define los1 empty)
(define los2 (cons "world" empty))
(define los3 (cons "hello" (cons "world" empty)))
(define los4 (cons "hello" (cons "world" los3)))

#;(define (los-temp alos)
    (cond [(empty? alos) ...]
          [(cons? alos) ... (first alos)
                        ... (los-temp (rest alos))]))

;; LOS -> Number
;; compute the string-length of all of the strings added together
(define (size alos)
  (cond [(empty? alos) 0]
        [(cons? alos) (+ (string-length (first alos))
                         (size (rest alos)))]))

(check-expect (size los1) 0)
(check-expect (size los2) 5)
(check-expect (size los3) 10)

;; how often ; LOS String -> Number
;; how often does the given string appear？
(define (how-often alos st)
    (cond [(empty? alos) 0]
          [(cons? alos) (cond [(string=? (first alos) st) 
                               (+ 1 (how-often (rest alos) st))]
                              [else (how-often (rest alos) st)])]))

(check-expect (how-often los1 "hello") 0)
(check-expect (how-often los2 "hello") 0)
(check-expect (how-often los4 "hello") 2)

;; replace : OS String String -> LLOS
;; replace the old string with the new one wherever it appears 
(define (replace alos old new)
    (cond [(empty? alos) empty]
          [(cons? alos) (if (string=? old (first alos)
                        (cons new (replace (rest alos) old new))
                        (cons old (replace (rest alos) old new))))]))

(check-expect (replace los1 "hello" "hi") empty)
(check-expect (replace los3 "hello" "hi")
              (cons "hi" (cons "world" empty)))
(check-expect (replace los4 "hello" "hi")
              (cons "hi" (cons "world" (cons "hi" (cons "world" empty)))))


              