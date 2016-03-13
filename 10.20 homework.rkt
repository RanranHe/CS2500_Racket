;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |10.20 homework|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;;=======Problem 1=======;;

;; 1. No value
;; 2. f
;; 3. No Value

;; Only 2 has a value because it calls on the function with the parenthesis,
;; the other questions do not.

;;=======Problem 2=======;;

;; fucntion=at-1-2-3-and-5.775? : [Number -> Number] [Number -> Number] -> Boolean
;; Determines if two functions from these numbers will produce the same results
;; (1.2, 3, and -5.775)
(define (fucntion=at-1-2-3-and-5.775? af bf)
  (if (and (= (af 1.2) (bf 1.2)) 
           (= (af 3) (bf 3)) 
           (= (af -5.775) (bf -5.775))) 
      true false))

;; fu1 : Number -> Number
;; caculate the number by the given function and return a new number
(define (fu1 x) (+ x 1))

;; fu2 : Number -> Number
;; caculate the number by the given function and return a new number
(define (fu2 x) (* 2 x))

(check-expect (fucntion=at-1-2-3-and-5.775? fu1 fu2) false)
(check-expect (fucntion=at-1-2-3-and-5.775? fu1 fu1) true)

;; function=? cannot be defined because we cannot 
;; determine whether all the numbers can make the two functions equal 

;;=======Problem 3=======;;

;; tabulate : [Number -> Number] Number -> [List-of Number]
;; tabulates the given operator between n and 0 (inclusive) in a list
(define (tebulate op n)
  (cond [(= n 0) (list (op 0))]
        [else (cons (op n)
                    (tebulate op (sub1 n)))]))

(check-expect (tebulate sqrt 0) (list 0))
(check-expect (tebulate sqrt 1) (list 1 0))

;; tabu-sqr : Number -> [List-of Number]
;; tebulate sqr between n and 0 (inclusive) in a list
(define (tabu-sqr n)
  (tebulate sqr n))

(check-expect (tabu-sqr 0) (list 0))
(check-expect (tabu-sqr 3) (list 9 4 1 0))

;; tabu-tan : Number -> [List-of Number]
;; tebulate tan between n and 0 (inclusive) in a list
(define (tabu-tan n)
  (tebulate tan n))

(check-expect (tabu-tan 0) (list 0))
(check-within (tabu-tan 2) (list -2 1.5 0) 0.2)
                                 

;;=======Problem 4=======;;

;; fold1 : Number [Number Number -> Number] [List-of Number] -> Number
;; compute the numbers in the list by using the given operator
(define (fold1 base op l)
  (cond [(empty? l) base]
        [else (op (first l)
                  (fold1 base op (rest l)))]))

(check-expect (fold1 0 + '(1 2 3 4)) 10)
(check-expect (fold1 1 - '()) 1)

;; fold2 : [X Y] Y [X Y -> Y] [List-of X] -> Y
;; apply op from left to right to each item in l
(define (fold2 noi op l)
  (cond [(empty? l) noi]
        [else (op (first l)
                  (fold2 noi op (rest l)))]))

;; Posn Image -> Image 
(define (place-dot p img)
  (place-image dot
               (posn-x p) (posn-y p)
               img))
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

(check-expect (fold2 0 + '(1 2 3 4)) 10)
(check-expect (fold2 0 - '()) 0)
(check-expect (fold2 emt place-dot (list (make-posn 10 20)
                                         (make-posn 2 10)
                                         (make-posn 30 20)))
 (place-image (circle 3 "solid" "red") 
              10 20  
              (place-image (circle 3 "solid" "red") 
                           2 10 
                           (place-image (circle 3 "solid" "red")
                                        30 20 
                                        (empty-scene 100 100)))))

;;=======Problem 5=======;;

;; [List-of [Number -> Number]] : 
;; A List of functions (input and output are both number)

;; lof ([List-of [Number -> Number]]) is one of :
;; empty
;; (cons [Number -> Number] lof)
#;(define (temp lof)
    (cond [(empty? lof) ...]
          [else ...(first lof)
                ...(rest lof)]))
;; at-0 : [List-of [Number -> Number]] -> [List-of Number]
;; produces the list of results of applying functions in the list to 0
(define (at-0 lof)
  (cond [(empty? lof) '()]
        [else (cons ((first lof) 0)
                    (at-0 (rest lof)))]))

;; f1 : Number -> Number
;; caculate the number in the given function and return a new number
(define (f1 n) 
  (+ (* 2 n) 1))

;; f2 : Number -> Number
;; caculate the number in the given function and return a new number
(define (f2 n)
  (- n 4))

;; f3 : Number -> Number
;; caculate the number in the given function and return a new number
(define (f3 n)
  (* 6 n))

(define l1 (list f1 f2 f3))
(check-expect (at-0 l1)
              (list 1 -4 0))

;;=======Problem 6=======;;

;; los ([List-of String]) is one of :
;; empty
;; (cons String los)
#;(define (temp los)
    (cond [(empty? los) ...]
          [else ...(first los)
                ...(rest los)]))

;; find-string : String [List-of String] -> Boolean
;; check if the given string is a member of the list
(define (find-string s los)
  (cond [(empty? los) false]
        [else (if (string=? s (first los))
                  true 
                  (find-string s (rest los)))]))

(check-expect (find-string "apple" (list "APPle" "is" "red")) false)
(check-expect (find-string "cat" (list "APPle" "is" "red")) false)
(check-expect (find-string "apple" (list "apple" "is" "red")) true)


;; generic-find-string : String [List-of String] [String -> String] -> Boolean
;; check if the given string is a member of the list
(define (generic-find-string s los p)
  (cond [(empty? los) false]
        [else (if (p s (first los))
                  true 
                  (generic-find-string s (rest los) p))]))

;; find-string-case-sensitive : String [List-of String] -> Boolean
;; check if the given string is a member of the list
(define (find-string-case-sensitive s los)
  (generic-find-string s los string=?))

(check-expect (find-string-case-sensitive "apple" (list "APPle" "is" "red")) false)
(check-expect (find-string-case-sensitive "apple" (list "apple" "is" "red")) true)

;; find-string-case-insensitive : String [List-of String] -> Boolean
;; check if the given string is a member of the list and ignores
;; the case of alphabetic characters when comparing strings
(define (find-string-case-insensitive s los)
  (generic-find-string s los string-ci=?))

(check-expect (find-string-case-insensitive "apple" (list "APPle" "is" "red")) true)
(check-expect (find-string-case-insensitive "IS" (list "apple" "is" "red")) true)
(check-expect (find-string-case-insensitive "dog" (list "APPle" "is" "red")) false)

