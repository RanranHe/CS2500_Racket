;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.15(3)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A book is a (make-book string string symbol Genre Number)
(define-struct book (author title genre date))

;; A Genre is one of:
;; 'sci-fi
;; 'mystery
;; 'horror

(define b1 (make-book "Wells" "The Time Machine" 'sci-fi 1895))
(define b2 (make-book "Stein" "Goosebumps" 'horror 1995))

#|
constructor: make-book
selectors:
    book-title
    book-author
    book-genre
    book-date
predicate: book?
|#

;; Book Number -> Booklean
;; determines if the book was published before a given date
(define (before? bk dt)
  (< (book-date bk) dt))

(check-expect (before? b1 1960) true)
(check-expect (before? b2 1990) false)

;; Book String Number -> Boolean 
;; Is the book with the givern title published before the given date 
(define (before2? bk t dt)
  (cond [(string=? (book-title bk) t) (< (book-date bk) dt)]
        [else false]))