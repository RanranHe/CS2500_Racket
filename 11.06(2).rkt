;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.06(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct person (name eye loc))
;; a person is a (make-person String Symbol [List-of Person]

;; [List-of Person] is one of :
;; empty
;; (cons Person [List-of Person])

;; Person -> [List-of String]
;; produce the list of names of all in the family tree
(define (names p)
  (cons (person-name p)
        (lop-names (person-loc p))))

;; LOP -> [List-of String]
(define [lop-temp alop]
  (cond [(empty? alop) empty]
        [else (cons (person-names (first alop))
                    (lop-names (rest alop))]))

(define flo (make-person "Flo" 'blue
                         (list (make-person "Bo" 'brown empty)
                               (make-person "Jo" 