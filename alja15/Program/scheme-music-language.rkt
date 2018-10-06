#lang racket
; Author: Anders Langballe Jakobsen <alja15@student.aau.dk>
; Study number: 20154059

; Utility functions from exercise solutions
; Written by Kurt Nørmark
(define (pair-up key-list value-list)
  (if (or (null? key-list) (null? value-list))
      '()
      (cons
       (cons (car key-list) (car value-list))
       (pair-up (cdr key-list) (cdr value-list)))))

; Basic music theory definitions
(define instrument-channels '((piano 1) (organ 2) (guitar 3) (violin 4) (flute 5) (trumpet 6) (helicopter 7) (telephone 8)))

; Helper functions
; Get the channel from an instrument
(define (instrument-channel instrument)
  (if (instrument? instrument)
      (assoc instrument instrument-channels)
      (error("Cannot get channel from something which is not an instrument."))))

; Check if an instrument is valid
(define (instrument? instrument)
  (let ((lookup (assoc instrument instrument-channels)))
    (pair? lookup)))

; Checks if a duration is valid
(define (duration? duration)
  (and (integer? duration)
       (>= duration 0)))

; Checks if a pitch value is valid
(define (pitch? value)
  (and (integer? value)
       (and (>= value 0)
            (<= value 127))))

; Checks if a type is a note type
(define (note-type? type)
  (eq? type 'note))

; Checks if a type is a pause type
(define (pause-type? type)
  (eq? type 'pause))

; Checks if a type is a sequence type
(define (seq-type? type)
  (eq? type 'sequential))

; Checks if a type is a parallel composition type
(define (par-type? type)
  (eq? type 'par))

; Checks if a type is a music type
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
           (let ((lookup (assoc 'type element)))
             (cond ((pair? lookup)
                    (eq? (cdr lookup) type))
                   (else #f))))
          (else #f))))

; Check if an element is a note.
(define is-element-note?
  (is-element-of-type 'note))

; Check if an element is a pause.
(define is-element-pause?
  (is-element-of-type 'pause))

; Check if an element is a sequence.
(define is-element-seq?
  (is-element-of-type 'seq))

; Check if an element is a parallel composition.
(define is-element-par?
  (is-element-of-type 'par))

; Accessor functions
(define (get-key key element)
  (cond ((list? element)
         (let ((lookup (assoc key element)))
           (cond ((pair? lookup)
                  (cdr lookup))
                 (else (error("Could not look up key from association list."))))))
        (else (error("Element is not an association list.")))))

; Gets the duration from an element, given that is it either a pause or a note
(define (get-duration element)
  (cond ((or (is-element-pause? element) (is-element-note? element))
         (get-key 'duration element))
        (else (error("Cannot get duration from something that is not a note or an element.")))))
            
; Constructor functions. This collection of functions is used to create the different music elements.
(define (make-pause pause-duration)
  (cond ((duration? pause-duration)
         (pair-up ('type 'duration) ('pause-type pause-duration))
        )
        (else error("Cannot make a pause element with an invalid duration."))))

(make-pause 10)
