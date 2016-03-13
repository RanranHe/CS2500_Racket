;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; 贪吃蛇 SNAKE
;; Width
;; Height
;; BG
;; SEG-RADIUS
;; DISTANCE AT EACH TICK
;; COLOR OF THE FOOD
;; COLOR-OF-THE-SNAKE

;; WORLD 
;; FOOD (STRUCTURE? POSN)
;; (MAKE-SNAKE(DIRECTION LIST-OF-POSN))
;; DIRECTION IS ONE OF : LEFT, RIGHT, UP, DOWN
;; (make-world snake food)