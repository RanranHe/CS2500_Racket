;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1007 P1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A letter is (make-letter String Number)
;; weight of the letter is measured in ounces
(define-struct letter (address weight))

;; A box is (make-box Number Number Number Number)
;; height, width and length of the box are measured in inches
;; weight of the box is measured in ounces
(define-struct box (height width length weight))

;; An Item is one of:
;; (make-letter String Number)
;; (make-box Number Number Number Number)
(define (item-ok? x)
  (cond [(letter? x) (if (< (letter-weight x) 3.5) "satisfied"
                         "not satisfied")]
        [(box? x) (if (and (<= (+ (box-height x) (box-width x) (box-length x)) 62)
                           (<= (box-weight x) 800)
                           (<= (* (box-height x) (box-width x) (box-length x)) 7938)) 
                      "satisfied"
                      "not satisfied")]))

(define le1 (make-letter "south" 5))
(define le2 (make-letter "south" 3))
(define b1 (make-box 0.1 20 50 700))
(define b2 (make-box 20 10 20 900))
(define b3 (make-box 20 20 20 700))
(define b4 (make-box 20 10 10 700))

(check-expect (item-ok? le1) "not satisfied")
(check-expect (item-ok? le2) "satisfied")
(check-expect (item-ok? b1) "not satisfied")
(check-expect (item-ok? b2) "not satisfied")
(check-expect (item-ok? b3) "not satisfied")
(check-expect (item-ok? b4) "satisfied")

;; List of mails -> List of not satisfied mails
;; A LOI is one of:
;; empty
;; (cons item LOI))
(define (bad-item loi)
  (cond [(empty? loi) empty]
        [(cons? loi) 
         (cond [(letter? (first loi))
                (if (< (letter-weight (first loi)) 3.5) 
                    (bad-item (rest loi))
                    (cons (first loi) (bad-item (rest loi))))]
               [(box? (first loi)) 
                (if (and (<= (+ (box-height (first loi)) (box-width (first loi)) (box-length (first loi))) 62)
                         (<= (box-weight (first loi)) 800)
                         (<= (* (box-height (first loi)) (box-width (first loi)) (box-length (first loi))) 7938)) 
                    (bad-item (rest loi))
                    (cons (first loi) (bad-item (rest loi))))])]))

(check-expect (bad-item (cons le1 (cons b4 empty))) (cons (make-letter "south" 5) empty))
(check-expect (bad-item (cons le1 (cons le2 (cons b3 empty))))
              (cons (make-letter "south" 5) (cons (make-box 20 20 20 700) empty)))
(check-expect (bad-item (cons b4 (cons le2 empty))) empty)
(check-expect (bad-item empty) empty)
(check-expect (bad-item (cons le1 (cons le2 (cons b1 (cons b2 (cons b4 (cons b3 empty)))))))
              (cons (make-letter "south" 5) 
                    (cons (make-box 0.1 20 50 700)
                          (cons (make-box 20 10 20 900)
                                (cons (make-box 20 20 20 700) empty)))))

;; List(LOI) -> Number(total postage)
;; letter-postage: 0.5
;; box-postage: 0.5*box-weight
;; total-postage: letter-postage + box-postage
(define (total-postage loi)
  (cond [(empty? loi) 0]
        [(cons? loi) 
         (cond [(letter? (first loi))
                (if (< (letter-weight (first loi)) 3.5)
                    (+ 0.5 (total-postage (rest loi)))
                    (total-postage (rest loi)))]
               [(box? (first loi)) 
                (if (and (<= (+ (box-height (first loi)) (box-width (first loi)) (box-length (first loi))) 62)
                         (<= (box-weight (first loi)) 800)
                         (<= (* (box-height (first loi)) (box-width (first loi)) (box-length (first loi))) 7938)) 
                    (+ (* 0.5 (box-weight (first loi))) (total-postage (rest loi)))
                    (total-postage (rest loi)))])]))

(check-expect (total-postage (cons le1 (cons le2 empty))) 0.5)
(check-expect (total-postage (cons le1 (cons le2 (cons b3 empty)))) 0.5)
(check-expect (total-postage (cons b4 (cons le2 empty))) 350.5)
(check-expect (total-postage empty) 0)
(check-expect (total-postage (cons le1 (cons le2 (cons b1 (cons b2 (cons b4 (cons b3 empty))))))) 350.5)
(check-expect (total-postage (cons le2 (cons b4 (cons (make-letter "bu" 2) (cons (make-box 10 10 10 100) empty))))) 401)
              

               

                      
                      
                     