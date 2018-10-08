#lang racket
; The accumulate higher order function is provided on Moodle
(define (accumulate f init lst)
  (if (null? lst)
      init
      (f (car lst) (accumulate f init (cdr lst)))))

; A higher order function which counts how many elements satisfy a predicate
(define (count pred lst)
  (count-help pred lst 0))

(define (count-help pred lst count)
  (cond ((null? lst) count)
        ((pred (car lst)) 
           (count-help pred (cdr lst) (+ count 1)))
        (else 
           (count-help pred (cdr lst) count))))
