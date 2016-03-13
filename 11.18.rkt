;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A node is a (make-node Symbol [List-of Symbol]
(define-struct node (name neighbors))

;; Graph = [List-of Node]

(define h1 (list (make-node 'A '(B))
                 (make-node 'B '(C))
                 (make-node 'C '(A E))
                 (make-node 'E '(D))
                 (make-node 'D '(()))))

;; neighbors : Graph Symbol -> [List-of Symbol]
;; find the list of the neighbors of the given symbol
(define (neighbors g n)
  (cond [(empty? g) (error "node doesn't exist")]
        [else (if (symbol=? n (node-name (first g)))
                  (node-neighbors (first g))
                  (neighbors (Rest g)))]))

(check-expect (neighbors g1 'C) '(A E))
(check-error (neighbors g1 'G) "node doesn't exist")

;; connected? : Graph Symbol Symbol -> Boolean
;; are the two nodes directly connected?
(define (connected? g a b)
  (member? b (neighbors g a)))

(check-expect (connected? g1 'A 'B) true)
(check-expect (connected? g1 'D 'C) false)
(check-error (connected? g1 'F 'A) "node doesn't exist")

;; path? : Graph Symbol Symbol -> Boolean
;;;; is there a path from the first node to the second?
(define (path? g a b)
  (cond [(symbol=? a b) true]
        [(connected? g a b) true]
        [else (ormap (λ (x) (path? g a x)) (neighbors g b))]))

(check-expect (path? g1 'A 'C) true)
(check-expect (path? g1 'E 'A) false)
        
                 