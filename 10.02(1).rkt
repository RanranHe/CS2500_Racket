;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.02(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A LOE is one of 
;; empty
;; (cons Entry LOE)

(define my-grades (cons 79 (cons 85 (cons 97 empty))))

;; A student-Rec is a (make-record String LON)
(define-struct record (name grades))
(define bob (make-record "bob" (cons 79 (cons 85 (cons 90 empty)))))
(define alice (make-record "alice" (cons 86 (cons 80 (cons 92 empty)))))

;; A student-Ave is a (make-ave String Number)
(define-struct ave (name grade))
(define bob-ave (make-ave "bob" 83.333))

;; Student-Rec -> Student-Ave
;; make a Student-Ave by computing the average grade of a student
(define (compute-average asr)
  (make-ave (record-name asr)
            (average (record-grades asr))))
  
(check-expect (compute-average (make-record "sally" (cons 100 empty)))
              (make-ave "sally" 100))

;; count: LON -> Number
;; how many nubers in the list?
(define (count alon)
  (cond [(empty? alon) 1]
        [(cons? alon) (+ 1 (count (rest alon)))]))
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

;; A LOR is one of:
;; empty
;; (cons Student-Rec LOR)

(define lor1 (cons bob (cons alice empty)))

;; A LOG is one of :
;; empty
;; (cons Student-Ave LOG
;;(define log1 (cons bob-ave (cons alice-ave empty)))

;; LOR -> LOG
;; compute the grades for all the students
(define (compute-grades alor)
  (cond [(empty? alor) empty]
        [(cons? alor) (cons (compute-average (first alor))
                            (compute-grades (rest alor)))]))