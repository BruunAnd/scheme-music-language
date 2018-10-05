#lang racket

; Basic music theory definitions
(define instruments '(piano organ guitar violin flute trumpet helicopter telephone))

; Helper functions
(define (note-type? type)
  (eq? type 'note))

(define (pause-type? type)
  (eq? type 'pause))

(define (seq-type? type)
  (eq? type 'sequential))

(define (par-type? type)
  (eq? type 'par))

(define (music-type? type)
  (or (note-type? type)
      (pause-type? type)
      (seq-type? type)
      (par-type? type)))

; All music elements in my music language are association lists
; As such, I can check if some list is a music element by checking its type.

; Helper function which returns a function for checking whether some
; element is of a type. Returns #f if the element is not a list, or
; if assq returns something that is not a pair.
(define (is-element-of-type type)
  (lambda (element)
    (cond ((list? element)
           (let ((lookup (assq 'type element)))
             (cond ((pair? lookup)
                    (eq? (cdr lookup) type))
                   (else #f))))
          (else #f))))

(define is-element-note
  (is-element-of-type 'note))

(define is-element-pause
  (is-element-of-type 'pause))

(define is-element-seq
  (is-element-of-type 'seq))

(define is-element-par
  (is-element-of-type 'par))

;(define q '((anders . linuxfag) (theis. macfag)))