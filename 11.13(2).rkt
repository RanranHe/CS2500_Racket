;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.13(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A Graph is a [List-of Node]
;; A Node is (cons Symbol [List-of Symbol])

(define make-node cons)
(define node-name first)
(define node-calls rest)

(define graph-a (list (make-node 'amy '(billy claire))
                      (make-node 'billy '(dan))
                      (make-node 'dan '(ali))
                      (make-node 'ali '(claire))
                      (make-node 'claire '())))
(define graph-b (list (make-node 'amy '(billy claire))
                      (make-node 'billy '(dan))
                      (make-node 'dan '(ali))
                      (make-node 'ali '(claire))
                      (make-node 'claire '(bob))))

;; named constraint pn a call-graph : 
;; does every person who was called have a node in the graph?

;; named? ; Graph -> Boolean
;; is everyone who was called a named node?
(define (name? agraph

(check-expect (named? graph-a) true)
(check-expect (named? graph-a) false)

;; all-the-node-names : Graph -> [List-of Symbol]
;; list all of the node names
(define (all-the-node-names agraph)
  (map node-name agraph))

(check-expect (all-the-node-names graph-a)
              '(amy billy dan ali claire))

;; all-the-calls : Graph -> [List-of Symbol]
;; list all of the people who were called
(define (all-the-calls agraph)
  (foldr (λ (x y) (append (node-calls x) y)) empty agraph))

(check-expect (all-the-calls graph-a)
              '(amy billy dan ali claire))
                                 