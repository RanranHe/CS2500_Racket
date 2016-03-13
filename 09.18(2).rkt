;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.18(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A professor is a (make-prof string sring symbol)
(define-struct prof (first firt course))

(define prof1 (make-prof "Clin" "Shivers" 'CS2500))
(define prof2 (make-prof "Leena" "Razzaq" 'CS2510))

;; A student is a (make-student String String Prof)
(define-struct student (first last prof))

(define student1 (make-student "Bob" "Smith" prof1))
(define student2 (make-student "Mary" "Jones" prof2))
(define student3 (make-student "Mary" "Jones" prof1))

;; student student --> Boolean
;; are the two students taking the same course?
(define (same-course? s1 s2)
  (symbol=? (prof-course (student-prof s1)) 
            (prof-course (student-prof s2))))

(check-expect (same-course? student1 student2) false)
(check-expect (same-course? student1 student3) true)