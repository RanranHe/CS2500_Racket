;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Number Number Fun Number -> Number
;; find the root of the function
(define (find-root lo hi f delta)
  (cond [(<= (- hi lo) delta) lo]
        [else (local [(define width (- hi lo))
                      (define num-windows (ceiling (/ width delta)))
                      (define (mk-window i) (+ lo (* i delta)))
                      (define intervals (build-list num-windows mk-window))
                      (define (good window)
                        (<= (* (f window) (f (+ window delta))) 0))]
                (first (filter good intervals)))]))
                
(check-expect (find-root -1 1 (λ (x) x) 0.1) -0.1)

;; Number Number [X -> Y] Number -> Number
;; find the root of the function by using binary search
(define (find-root2 lo hi f delta)
  (cond [(<= (- hi lo) delta) lo]
        [else (local [(define mid (/ (+ hi lo) 2))
                      (define f@mid (f mid))
                      (define f@lo (f lo))
                      (define f@hi (f hi))]
              (cond [(<= (x f@mid f@lo)
                         (find-root2 lo mid f delta))]
                    [else (find-root2 mid min hi f delta)]))]))

;; Natural Natural -> Natural
;; find the gcd of the two numbers
(define (gcds x y)
  (local [(define (first-div i)
            (cond [(= i 1) 1]
                  [else (if (and (= (remainder x div) 0)
                                 (= (remainder y div) 0))
                            i
                            (first-div (sub1 i)))]))]
    (first-div (min x y))))
          

(check-expect (gcds 24 18) 6)
(check-expect (gcds 23 17) 1)
                        
;; gcdg : Natural Natural -> Natural
;; find the gcd using euclid's algorithm
;; using the remainder whne we divide larger by smaller
;; (gcdg 24 18) -> (gcd 18 6) -> (gcd 6 0) ans: 6
(define (gcdg x y)                       
  (local [(define (clever-gcd larger smaller)
            (cond [(= smaller 0) larger]
                  [else (clever-gcd smaller (remainder larger smaller))]))]
    (clever-gcd (max x y) (min x y))))