;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |09.29(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; An Entry is a (make-entry String Number)
(define-struct entry (name num))

(define bob (make-entry "Bob" 123))
(define alice (make-entry "Alice" 456))
(define sally (make-entry "Sally" 789))

;; A LOE is one of 
;; empty
;; (cons Entry LOE)

(define loe1 empty)
(define loe2 (cons bob empty))
(define loe3 (cons alice (cons sally loe2)))

;; A LOS is one of:
;; empty
;; (cons String LOS)

;; find: LOE String -> Number
;; find the number associated with the given name
(define (find aloe name)
  (cond [(empty? aloe) (error "Name not found")]
        [(cons? aloe) (if (who-is-it? name (first aloe))
                          (entry-num (first aloe))
                          (find (rest aloe) name))]))
  

(check-error (find loe1 "Bob") "Name not found")
(check-expect (find loe2 "Bob") 123)
(check-expect (find loe3 "Bob") 123)

;; Entry -> Boolean
;; is there a match
(define (who-is-it? name ent)
  (string=? (entry-name ent) name))

;; LOE -> LOS
;; extract the names from the list of entries
(define (names aloe)
  (cond [(empty? aloe) empty]
        [(cons? aloe) (cons (entry-name (first aloe))
                            (names (rest aloe)))]))

(check-expect (names loe1) empty)
(check-expect (names loe3) (cons "Alice" (cons "Sally" (cons "Bob" empty))))

;; A Contacts is a (make-contacts String LOE)
(define-struct contacts (name phone-book))

(define friends (make-contacts "friends" loe3))
(define coworkers (make-contacts "coworkers" empty))

;; String Contacts -> Contacts
;; delete a contact with the given name
(define (remove-entry st con)
  (make-contacts (contacts-name con)
                 (delete-entry st (contacts-phone-book con))))

(check-expect (remove-entry "Mary" friends) friends)
(check-expect (remove-entry "Sally" friends) 
              (make-contacts "friends" (cons alice (cons bob empty))))

;; String LOE -> LOE
;; remove the given entry from the list
(define (delete-contast st aloe)
  (cond [(empty? aloe) empty]
        [(cons? aloe) (if (who-is-it st (first aloe))
                          (rest aloe)
                          (cons (first aloe) (delete-entry st (rest aloe))))]))

(check-expect (delete-entry "Mary" loe3) loe3)
(check-expect (delete-entry "Sally" loe3) (cons alice (cons bob empty)))
              


