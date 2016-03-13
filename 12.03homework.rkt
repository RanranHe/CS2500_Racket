;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 12.03homework) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Exercise 32.2.1.

(define-struct state (left-mis left-can boat right-mis right-can))
;; a state is (make-state Number Number Symbol Number Number)

;; initial state :
;; (make-state 3 3 'left 0 0)

;; final state :
;; (make-state 0 0 'right 3 3)

;;=======================================================================
;;Exercise 32.2.2.

(define-struct loads (mis can))
;; a loads is (make-loads (Number Number)

;; list of all possible boat loads
(define BOAT-LOADS (list (make-loads 2 0)
                         (make-loads 1 1)
                         (make-loads 0 2)
                         (make-loads 1 0)
                         (make-loads 0 1)
                         (make-loads 0 0)))

;; make-BOAT-LOADS : Number -> [list-of loads]
;; returns a list of possible (cannibals cannot be more than missionaries)
(define (make-BOAT-LOADS mc)
  (local [;;give all kinds of load that the total number is same as max-capacity
          ;;accumulator: track the number of cannibals in load so far
          (define (g mc acc)
            (cond [(< mc 1) (list (make-loads 0 acc))]
                  [else (cons (make-loads mc acc)
                              (g (- mc 1) (add1 acc)))]))
          ;;give all kinds of load that the total number
          ;;is equal or less than max-capacity
          ;;accumulator: track the number of cannibals in load so far
          (define (f mc acc)
            (cond [(< mc 1) (list (make-loads 0 acc))]
                  [else (append (g mc acc)
                                (f (sub1 mc) acc))]))
          (define lol (f mc 0))]
  (filter (λ (x) (or (>= (loads-mis x) (loads-can x))
                     (= (loads-mis x) 0))) lol))) 

(check-expect (make-BOAT-LOADS 4)
              (list (make-loads 4 0) (make-loads 3 1)
                    (make-loads 2 2) (make-loads 0 4)
                    (make-loads 3 0) (make-loads 2 1) 
                    (make-loads 0 3) (make-loads 2 0)
                    (make-loads 1 1) (make-loads 0 2)
                    (make-loads 1 0) (make-loads 0 1)
                    (make-loads 0 0)))
(check-expect (make-BOAT-LOADS 0)
              (list (make-loads 0 0)))

;;=======================================================================
;;Exercise 32.2.3.
;; successor-states : State -> [List-of States]
;; consumes a list of states, and returns a list of 
;; states reachable with one crossing
(define (successor-states s)
  (local [(define boats (filter (λ (x) (not (and (= (loads-mis x) 0)
                                                 (= (loads-can x) 0))))
            (make-BOAT-LOADS 2)))
          (define (successor-states.v2 s boats)
            (cond 
              [(empty? boats) (list s)]
              [(symbol=? 'left (state-boat s))
               (cons (make-state 
                      (- (state-left-mis s) (loads-mis (first boats)))
                      (- (state-left-can s) (loads-can (first boats)))
                      'right
                      (+ (state-right-mis s) (loads-mis (first boats)))
                      (+ (state-right-can s) (loads-can (first boats))))
                     (successor-states.v2 s (rest boats)))]
              [(symbol=? 'right (state-boat s))
               (cons (make-state 
                      (+ (state-left-mis s) (loads-mis (first boats)))
                      (+ (state-left-can s) (loads-can (first boats)))
                      'left
                      (- (state-right-mis s) (loads-mis (first boats)))
                      (- (state-right-can s) (loads-can (first boats))))
                     (successor-states.v2 s (rest boats)))]))]
    (successor-states.v2 s boats)))
                   
(check-expect (successor-states (make-state 6 5 'right 2 2))
              (list (make-state 8 5 'left 0 2)
                    (make-state 7 6 'left 1 1)
                    (make-state 6 7 'left 2 0)
                    (make-state 7 5 'left 1 2)
                    (make-state 6 6 'left 2 1)
                    (make-state 6 5 'right 2 2)))
(check-expect (successor-states (make-state 4 3 'left 0 0))
              (list (make-state 2 3 'right 2 0)
                    (make-state 3 2 'right 1 1)
                    (make-state 4 1 'right 0 2)
                    (make-state 3 3 'right 1 0)
                    (make-state 4 2 'right 0 1)
                    (make-state 4 3 'left 0 0)))

;;=======================================================================
;;Exercise 32.2.4.
;; legal? : State -> Boolean
;; determine whether a state is legal
(define (legal? s) 
  (and (>= (+ (state-left-mis s) (state-right-mis s))
           (+ (state-left-can s) (state-right-can s)))
       (or (>= (state-left-mis s) (state-left-can s))
           (= (state-left-mis s) 0))
       (or (>= (state-right-mis s) (state-right-can s))
           (= (state-right-mis s) 0))
       (>= (state-left-mis s) 0)
       (>= (state-left-can s) 0)
       (>= (state-right-mis s) 0)
       (>= (state-right-can s) 0)))

(check-expect (legal? (make-state 0 3 'right 4 0)) true)
(check-expect (legal? (make-state 0 3 'right 0 0)) false)
(check-expect (legal? (make-state -1 0 'left 4 2)) false)
(check-expect (legal? (make-state 5 3 'right 0 1)) true)

;; List-of States examples:
(define los1 (list (make-state 8 5 'left 0 2)
                   (make-state 7 6 'left 1 1)
                   (make-state 6 7 'left 2 0)
                   (make-state 7 5 'left 1 2)
                   (make-state 6 6 'left 2 1)
                   (make-state 6 5 'right 2 2)))
;; legal-states : [List-of States] -> [List-of States]
;; consumes a list of states and returns the sublist of legal states
(define (legal-states los)
  (filter legal? los))

(check-expect (legal-states los1) 
              (list (make-state 8 5 'left 0 2)
                    (make-state 7 6 'left 1 1)
                    (make-state 6 6 'left 2 1)
                    (make-state 6 5 'right 2 2)))
(check-expect (legal-states empty) empty)