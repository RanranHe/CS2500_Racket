;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.21(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; [List-of X] -> [List-of X]
;; reverse the order of the items in the list
(define (reversel alox)
  (cond [(empty? alox) empty]
        [else (append (reversel (rest alox))
                      (list (first alox)))]))

(check-expect (reversel '(a b c d)) '(d c b a))
(check-expect (reversel '()) '())

(reversel '(a b c))
(append (reversel '(b c)) (list 'a))
(append (append (reversel '(c)) (list 'b)) (list 'a))
(append (append (append (reversel '()) (list 'c)) (list 'b)) (list 'a))
(append (append (append empty '(c)) '(b)) '(a))
(append (append '(c) '(b)) '(a))
(append '(c b) '(a))
'(c b a)

;; reverse2 : [List-of X] -> [List-of X]
;;reverse the order of the items
(define (reverse2 alox)
  (local [;; accumulator : List reversed so far
          (define (r alox acc)
            (cond [(empty? alox) acc]
                  [else (r (rest alox) (cons (first alox) acc))]))]
    (r alox '())))
