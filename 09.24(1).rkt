;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.24(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A RD is one of:
;; 'solid
;; (make-doll RD)
(define-struct doll (contents))

(define d1 'solid)
(define d2 (make-doll 'solid))
(define d3 (make-doll (make-doll (make-doll 'solid))))

;; count-layers : RD -> Number
;; how many layers does the doll have?
(define (count-layers d)
  (cond [(symbol? d) 1]
        [(doll? d) (+ 1 (count-layers (doll-contents d)))]))

(check-expect (count-layers d1) 1)
(check-expect (count-layers d2) 2)
(check-expect (count-layers d3) 4)
         