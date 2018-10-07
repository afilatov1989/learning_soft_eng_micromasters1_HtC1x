;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname design_quiz2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural

; Problem 1:
; 
; Consider the above data definition for the age of a person.
; 
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).

;; Age -> Boolean
;; produces true if the age is between 13 and 19 inclusive
(check-expect (teenager? 10) false)
(check-expect (teenager? 13) true)
(check-expect (teenager? 15) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 25) false)

;(define (teenager? a) true) ; this is the stub

; <use a template from Age>

(define (teenager? a)
  (and (<= a 19) (>= a 13)))

; Problem 2:
; 
; Design a data definition called MonthAge to represent a person's age
; in months.

;; MonthAge is Natural
;; interp. the age of a person in months
(define MA0 120)
(define MA1 240)

#;
(define (fn-for-month-age ma)
  (... ma))

;; Template rules used:
;; - atomic non-distinct: Natural

; Problem 3:
; 
; Design a function called months-old that takes a person's age in years 
; and yields that person's age in months.
;

;; Age -> MonthAge
;; produces person's age in months by her age in years
(check-expect (months-old 10) 120)
(check-expect (months-old 0) 0)

;(define (months-old a) 10) ; this is the stub

; <use a template from Age>

(define (months-old a)
  (* 12 a))

; Problem 4:
; 
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
; 
; - if they are dead (which is shockingly poor health)
; - if they are alive then they can have 0 or more extra lives
; 
; Design a data definition called Health to represent the health of your
; character.
; 
; Design a function called increase-health that allows you to increase the
; lives of a character. The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.

;; Health is one of:
;; - false
;; - Natural
;; interp. false means dead, natural is the number of extra lives

(define H1 false)
(define H2 0)
(define H3 3)

#;
(define (fn-for-health h)
  (cond [(false? h) (...)]
        [(number? h) (... h)]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - atomic non-distinct: Natural

;; Functions

;; Health -> Health
;; produces a new Health value after increasing extra lives by 1
(check-expect (increase-health false) false)
(check-expect (increase-health 0) 1)
(check-expect (increase-health 5) 6)

;(define (increase-health h) 5) ; this is a stub

; <use a template from Health>

(define (increase-health h)
  (cond [(false? h) false]
        [(number? h) (+ 1 h)]))