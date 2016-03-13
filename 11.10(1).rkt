;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; a BT is one of
;; 'leaf
;; (make-node BT BT)
(define-struct node (left right))

;; Natural -> [List-of BT]
;; list all the bt of height n
(define (all-bts n)
  (cond [(zero? n) (list 'leaf)]
        [else (append (cross (all-bts (sub1 n)) (all-bts (sub1 n)))
                      (cross (all-bts (sub1 n)) (all-bts< (sub1 n)))
                      (cross (all-bts< (sub1 n)) (all-bts (sub1 n))))]))

;; all-bt< : Natural -> [List-of BT]
;; all bts of height less than n
(define (all-bts< n)
  (cond [(zero? n) empty]
        [else (append (all-bts (sub1 n)) (all-bts< (- n 1)))]))

;; cross : [List-of BT] [List-of BT] -> [List-of BT]
;; cross all of the subtrees of height n-1 with those of height <n-1
(define (cross lbt1 lbt2)
  (cond [(empty? lbt1) empty]
        [else (append (add-bt (first lbt1) lbt2)
                      (cross (rest lbt1) lbt2))]))

;; add-bt : BT [List-of BT] -> [List-of BT]
;; add the tree to every member of the list
(define (add-bt abt lbt)
  (cond [(empty? lbt) empty]
        [else (cons (make-node (first lbt))
              (add-bt abt (rest lbt)))]))
  

