;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
constructors:
;; make-lecture-hall
selectors:
;; lecture-hall-number
;; lecture-hall-capacity
;; A lecture-hall is (make-lecture-hall Number String)
(define-struct lecture-hall (number capacity))
(define (lecture-hall-temp x)
  (cond [(number? (lecture-hall-number x)) ...]
        [(string? (lecture-hall-capacity x)) ...]))

constructors:
;; make-automobile
selectors:
;; automobile-year
;; automobile-make
;; automobile-model
;; A automobile is (make-automobile Number String Symbol)
(define-struct automobile (year make model))
(define (automobile-temp y)
  (cond [(number? (automobile-year y)) ...]
        [(string? (automobile-make y)) ...]
        [(symbol? (automobile-model y)) ...]))

constructors:
;; make-football-player  
selectors:
;; football-player-name
;; football-player-position
;; football-player-number
;; A football-player is (make-football-player String Posn Number)
(define-struct football-player (name position number))
(define (football-player-temp z)
  (cond [(string? (football-player-name z)) ...]
        [(posn? (football-player-position z)) ...]
        [(number? (football-player-number z)) ...]))

constructors:
;; make-shirt
selectors:
;; shirt-material
;; shirt-size
;; shirt-color
;; A shirt is (make-shirt Symbol Number String)
(define-struct shirt (material size color))
(define (shirt-temp a)
  (cond [(symbol? (shirt-material a)) ...]
        [(number? (shirt-size a)) ...]
        [(string? (shirt-color a)) ...]))