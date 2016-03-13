;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |10.16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
;; A LON is one of :
;; empty
;; (cons Number LON

;; Number LON -> LON
;; add the numbers in the list together
#|
(define (add-up alon)
  (cond [(empty? alon) 0]
        [(cons? alon) (+ (first alon)
                         (add-up (rest alon)))]))
|#
(define (add-up alon)
  (process + 0 alon))

(check-expect (add-up '(1 2 3 4)) 10)
(check-expect (add-up empty) 0)

;; mult: LON -》 NUmber
;; multiply the numbers in the list together
#|
(define (mult alon)
  (cond [(empty? alon) 1]
        [(cons? alon) (* (first alon)
                         (mult (rest alon)))]))
|#
(define (mult alon)
  (process * 1 alon))

(check-expect (mult '(1 2 3 4)) 24)
(check-expect (mult empty) 1)

;; process: OP X LOX -> 
;; process the members of the list using the given operator
(define (process op base alon)
  (cond [(empty? alon) base]
        [(cons? alon) (op (first alon)
                          (process op base (rest alon)))]))

(check-expect (process + 0 '(1 2 3 4)) 10)

;; draw-dots: LOP -> Image
;; draw a dot at each posn on the scene
(define (draw-dots alop)
  (process draw-dot (empty-scene 300 300) alop))

;; evens-list : LON -> LON
;; produce the list of even numbers that are in the original list
(define (evens-list alon)
  (cond [(empty? alon) empty]
        [else (if (even? (first alon))
                  (cons (first alon) (evens-list (rest alon)))
                        (evens-list (rest alon)))]))

(check-expect (evens-list '(0 1 2 3 4)) '(0 2 4))
(check-expect (evens-list empty) empty)

;; integers-list : LON -> LON
;; produce the list of integers
(define (integers-list alon)
  (cond [(empty? alon) empty]
        [else (if (integer? (first alon))
                  (cons (first alon) (integers-list (rest alon)))
                        (integers-list (rest alon)))]))

(check-expect (integers-list '(0 1 2.5 3.3 4)) '(0 1 4))
(check-expect (integers-list empty) empty)

;; filter-it : [X -> Boolean] LOX -> LOX
;; filter the list to the items that pass the test
(define (filter-it pred? alon)
  (cond [(empty? alon) empty]
        [else (if (pred? (first alon))
                  (cons (first alon) (filter-it pred? (rest alon)))
                        (filter-it pred? (rest alon)))]))

;; posns-in : LOP -> LOP
;; produce the posns where the x position is between 0 and 100
(define (posns-in lop)
  (local [;; Posn -> Boolean
          ;; is the x-position > 0 and < 100
          (define (in-range? p)
            (and (> (posn-x p) 0)
                 (< (posn-x p) 100)))]
    (filter in-range? lop)))

(check-expect (posns-in (list (make-posn -5 25)
                              (make-posn 60 49)
                              (make-posn 47 120)))
              (list (make-posn 60 49)
                    (make-posn 47 120)))

;; in-range? : Posn -> Boolean
;; Is the x position of the posn in range?
(define (in-range? p)
  (and (> (posn-x p) 0)
       (< (posn-x p) 100)))

;; all-over-5 : LON -> LON
;; produce all the number over 5
(define (all-over-5 alon)
  (filter-it over5? alon))

(check-expect (all-over-5 '(1 4 5 7 8 9)) '(7 8 9))

;; over5? : Number -> Boolean
;; is the number over 5?
(define (over5? n)
  (> n 5))

;; filter : [X -> Boolean] LOX -> LOX

;; map: [X -> Y] [List-of X] -> [List-of Y]
;; apply the operator to every member of the list

;; add5 : [List-of Number] -> [List-of Number]
(define (add5 op alon)
  (local [;; Number -> Number
          ;; adds 5 to n
          (define (add5-to-num n)
            (+ 5 n))]
    (map add5-to-num alon)))

(check-expect (add5 add5-to-num '(1 2 3)) '(6 7 8))

;; add5-to-num : Number -> Number
;; add 5 to n
(define (add5-to-num n)
  (+ 5 n))

;; List-images : [List-of Number] -> [List-of Image]
;; make a list of circles with radius taken from the given list
(define (list-images aloi)
  (map make-a-circle aloi))

(check-expect (list-images '(30 40 50))
              (list (circle 30 'solid 'red)
                    (circle 40 'solid 'red)
                    (circle 50 'solid 'red)))

;; Number -> Image
;; make a circle with the given radius
(define (make-a-circle r)
  (circle r 'solid 'red))

;; ormap : [X -> Boolean] [List-of X] -> Boolean
;; checks if at least one lement in the list passes a test

(local [(define (f x) (+ x 5))
        (define (g alon)
          (cond [(empty? alon) empty]
                [else (cons (f (first alon))
                            (g (rest alon)))]))]
  (g (list 1 2 3)))
        

#;(local [ defn 1
           defn 2
           ...
           defn n] exp)

(define y 10)
(+ y (local [(define y 5)
             (define x (+ y y))]
       x))
       
;; + : Nimber Number -> Number
;; * : Number Number -> Number
;; overlay : Image Image -> Image
;; string-append: String String -> String
(define cl (circle 10 'solid 'orange))

;; draw-dot: Posn Image -> Image
;; draw a circle at the location of the posn onto a scene
(define (draw-dot p i)
  (place-image cl (posn-x p) (posn-y p) i))

(check-expect (draw-dot (make-posn 30 40) (empty-scene 300 300))
              (place-image cl 30 40 (empty-scene 300 300)))

;; A LOI is one of 
;; empty
;; (Cons Image LOI)

(define loi1 (list (circle 50 'solid 'red)
                   (circle 100 'solid 'blue)))

;; overlay-loi: LOI -> Image
;; overlay the images in the list
#|
(define (overlay-loi alon)
  (cond [(empty? alon) (empty-scene 300 300)]
        [(cons? alon) (overlay (first alon)
                               (overlay-loi (rest alon)))]))
|#
(define (overlay-loi aloi)
  (process overlay (empty-scene 300 300) aloi))


(check-expect (overlay-loi loi1)
              (overlay (circle 50 'solid 'red)
                       (circle 100 'solid 'blue)
                       (empty-scene 300 300)))


#;(define (add-up alon)
    (cond [(empty? alon) ...]
          [(cons? alon) ...(first alon)
                        ...(add-up (rest alon))]))

;; foldr - X Y Y LOX -> Y 



