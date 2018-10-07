;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname design_quiz1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 
; PROBLEM:
; 
; Design a function that consumes two images and produces true
; if the first is larger than the second.
;

;; Image Image -> Boolean
;; produces true if the area (width * height) of the first image is larger than the area of the second one
(check-expect (image-larger? (rectangle 4 2 "solid" "red") (rectangle 1 8 "solid" "red")) false)
(check-expect (image-larger? (rectangle 4 2 "solid" "red") (rectangle 2 8 "solid" "red")) false)
(check-expect (image-larger? (rectangle 4 2 "solid" "red") (rectangle 1 1 "solid" "red")) true)

;(define (image-larger? img1 img2) true) ;stub

;(define (image-larger? img1 img2) ;template
; (... img1 img2)

(define (image-larger? img1 img2) 
  (> (* (image-height img1) (image-width img1))
     (* (image-height img2) (image-width img2))))