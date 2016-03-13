;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.21|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1
;; 1.
;; mak-palindrome: String -> String
;; consumes a non-empty String and constructs a 
;; palindrome by mirroring the String around the last letter
(define (make-palindrome s)
  (local [(define sl (string-length s))
          ;; mirror-string: Number String-> String
          ;; consumes a String-length n, a accumulator String ""and produce 
          ;; the right side of the mirroring String
          ;; accumulator: tracks previously appended String
          (define (mirror-string n acc)
            (cond [(zero? (sub1 n)) acc]
                  [else (mirror-string
                         (sub1 n)
                         (string-append acc (substring s (- n 2) (- n 1))))]))]
    (string-append s (mirror-string sl ""))))

(check-expect (make-palindrome "fundies") "fundieseidnuf")
(check-expect (make-palindrome "Hello") "HellolleH")
(check-expect (make-palindrome "a") "a")
(check-expect (make-palindrome "ab") "aba")
;;;;;;;;
;; 2.
;; is-palindrome?: String -> Boolean
;; consumes a non-empty String and determines 
;; whether the String is a palindrome or not
(define (is-palindrome? s)
  (and (odd? (string-length s)) ;;;;;;;;
       (string=? (make-palindrome (substring s 0 (add1 (quotient (string-length s) 2)))) s)))

(check-expect (is-palindrome? "fundieseidnuf") true)
(check-expect (is-palindrome? "a") true)
(check-expect (is-palindrome? "asddfs") false)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2
;; 1.
;; prime?: Natural -> Boolean
;; consumes a Natural Number, n and returns 
;; true if it is prime and false otherwise
(define (prime? n)
  (local [;; find-prime: Natural Natural -> Boolean
          ;; consumes a Natural Number, n and a Natural Number of the integer of 
          ;; the square root of n, and teturns true if it is prime and false ohterwise
          ;; accumulator: Track the integer square root of n so far
          (define (find-prime n m)
            (cond [(and (zero? (remainder n m)) (= 1 m)) true]
                  [(and (zero? (remainder n m)) (not (= 1 m))) false]
                  [else (find-prime n (sub1 m))]))]
    (find-prime n (integer-sqrt n))))

(check-expect (prime? 2) true)
(check-expect (prime? 10) false)

;; 2.
;; list-primes : Natural -> [List-of Natural]
;; consumes a Natural Number, n, and produces the list of prime numbers up to n
(define (list-primes n)
  (local [;; find-prime.v2: consumes a Natural Number n and a Naturral Number m,
          ;; find and list all the prime number that up to n
          ;; accumulator: Track the prime so far
          (define (find-prime.v2 n m)
            (cond [(= 1 m) (list 1)]
                  [(and (zero? (remainder n m))
                        (prime? m)) (append (list m) (find-prime.v2 n (sub1 m)))]
                  [else (find-prime.v2 n (sub1 m))]))]
    (cond [(prime? n) (list 1)]
          [else 
           (find-prime.v2 n n)])))

(check-expect (list-primes 15) (list 5 3 1))
(check-expect (list-primes 5) (list 1))
(check-expect (list-primes 8) (list 2 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
;; 1.
;; fibonacci: Natural -> Natural
;; consumes a Natural Number and produces its Fibonacci number
(define (fibonacci n)
  (cond [(zero? n) 0]
        [(= 1 n) 1]
        [else (+ (fibonacci (- n 2)) (fibonacci (sub1 n)))]))
(check-expect (fibonacci 9) 34)
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)

;; 2.
;; fibonacci.v2: Natural -> Natural
;; consumes a Natural Number and produces its Fibonacci number.
(define (fibonacci.v2 n)
  (local [;; find-fib: Natural Natural Natural -> Natural
          ;; consumes two Natural Number, x y and a accumulator, acc
          ;; produces its Fibonacci Number
          ;; accumulator: Track the 
          (define (find-fib x y acc)
            (cond [(zero? acc) x]
                  [else (find-fib y (+ x y) (sub1 acc))]))]
    (find-fib 0 1 n)))

(check-expect (fibonacci.v2 100) 354224848179261915075)
(check-expect (fibonacci.v2 9) 34)
(check-expect (fibonacci.v2 11) 89)
(check-expect (fibonacci.v2 0) 0)
(check-expect (fibonacci.v2 1) 1)

;; 3.
;; list-fibonacci: Natural -> [List-of Natural]
;; consumes a Natural Number and produces the list 
;; of Fibonacci numbers from F0 to Fn
(define (list-fibonacci n)
  (local [;; consume a  list, lof and a accumulator, acc
          ;; and produces the list of Fibonacci numbers from F0 to Fn
          (define (list-fib lof acc)
            (cond [(= -1 acc) lof]
                  [else (list-fib (append (list (fibonacci.v2 acc)) lof) (sub1 acc))]))]
    (list-fib '() n)))

(check-expect (list-fibonacci 5) '(0 1 1 2 3 5))
(check-expect (list-fibonacci 0) '(0))
(check-expect (list-fibonacci 1) '(0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4
;; 1.



(define-struct card (value suit))
;; a card is: (make-card number number)

;; where card-value is one of:
;; 1 to 13 (represent Ace to King)

;; a card suit is one of:
;; 1 to 4 (represent Clubs Diamonds Hearts and Spades)
;; where Clubs < Diamonds < Hearts < Spades

(define-struct trick (east south west north))
;; a trick is: (make-trick card card card card)

;; find-winner: Trick -> Card
;; consumes a trick and returns the wiining player's card
#;(define (find-winnder atrick)
  (local [;; card<: Card Card -> Card
          ;; consumes two cards, and returns the card with large value
          ;; by compare their value first, if same then compare their suit
          (define (card< c1 c2)
            (cond [(< (card-value c1) (card-value c2)) c2]
                  [(> (card-value c1) (card-value c2)) c1]
                  [(= (card-value c1) (card-value c2)) (if (> (card-suit c1) (card-suit c2))
                                                           c1
                                                           c2)]))
          ;;compare-card:
          ;;
          (define (compare-card atr acc)
            (card< (trick-west (card< (trick-south atr) acc))))]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5

;; 1.
(define-struct user (handle tweeps))
;; A tweeps is one of:
;; empty
;; (list String [List-of String])

;; A user is: (make-user string tweeps)
;; where handle is the name of twitter use
;; and tweeps is the follower of user

;; A Network is one of:
;; empty
;; (list tuser [List-of tuser])

(define network1
  (list (make-user "user1" (list "user2" "userx"))
        (make-user "user2" (list "userv" "user1" "user3"))
        (make-user "user3" '())
        (make-user "user4" '())
        (make-user "userv" (list "user2" "user4"))))




;; 2.
;; list-handles: Network -> [List-of String]
;; consumes a network and produces a list of
;; all of the handles in the network
(define (list-handles nw)
  (local [(define (get-names anw acc)
            (cond [(empty? anw) acc]
                  [else (get-names (rest anw) (cons (user-handle (first anw)) acc))]))]
    (get-names nw '())))
            
(check-expect (list-handles network1) (list "userv" "user4" "user3" "user2" "user1"))
(check-expect (list-handles empty) empty)

;; 3.
;; most-followers: Network -> String
;; consumes a network and produces the 
;; handle in the network that has the most followers.
(define (most-follower nw)
  (local [;;
          (define (find-most ntw acc)
            (cond [(empty? ntw) (user-handle acc)]
                  [else (if (< (length (user-tweeps (first ntw)))
                               (length (user-tweeps acc)))
                            (find-most (rest ntw) acc)
                            (find-most (rest ntw) (first ntw)))]))]
    (find-most (rest nw) (first nw)))) 
          
(check-expect (most-follower network1) "user2")


;; 4. 
;; friends?: Network String String -> Boolean
;; onsumes a network and two handles and 
;; determines whether it contains two users who follow each other
(define (friends? nw s1 s2)
  (local [(define (g nw s1 s2 acc)
            (cond [(empty? nw) false]
                  [else (
            