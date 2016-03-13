;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.19 homework|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Problem Set 11

;;===========Problem 1============;;
;; make-palindrome : String -> String
;; consumes a non-empty String and constructs a palindrome 
;; by mirroring the String around the last letter
(define (make-palindrome s)
  (local [(define num (string-length s))
          ;; string-mirror : String -> [List-of String]
          ;; mirror the string around the last letter into a list 
          (define (g s acc)
            (cond [(= 1 num) s]
                  [(= 2 num) (string-append s acc)]
                  [else (string-append s (g (substring s 1 num) (string-append (substring s 1 2) acc)))]))]
  (g s (substring s 0 1))))

(check-expect (make-palindrome "a") "a")
(check-expect (make-palindrome "abc") "abcba")

;; is-palindrome? : String -> Boolean
;; determines whether the String is a palindrome or not
(define (is-palindrome? s)
  (cond [(= 1 (string-length s)) true]
        [(even? (string-length s)) false]
        [(= (string-length s) 3) (string=? (substring s 0 1) (substring s 2 3))]
        [else (if (string=? (substring s (- (quotient (string-length s) 2) 1)
                                       (quotient (string-length s) 2))
                            (substring s (+ 1 (quotient (string-length s) 2))
                                       (+ (quotient (string-length s) 2) 2)))
                  (is-palindrome? (string-append (substring s 0 (- (quotient (string-length s) 2) 1))
                                                 (substring s (quotient (string-length s) 2) (+ 1 (quotient (string-length s) 2)))
                                                 (substring s (+ (quotient (string-length s) 2) 2) (string-length s))))
                  false)]))

(check-expect (is-palindrome? "aba") true)
(check-expect (is-palindrome? "a") true)
(check-expect (is-palindrome? "ab") false)
(check-expect (is-palindrome? "abc") false)
(check-expect (is-palindrome? "bcdcb") true)
(check-expect (is-palindrome? "bcwdacb") false)


;;===========Problem 2============;;    
