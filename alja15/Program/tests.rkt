; Author: Anders Langballe Jakobsen <alja15@student.aau.dk>, study number: 20154059
#lang racket

(require "scheme-music-language.rkt")

(define (assert expression)
  (if expression (display "Passed\n") (error "Failed")))

; The use of this file is deprecated. I have most of my tests in the usage file.
; Test pitch retrieval
(assert (eq? (get-pitch-from-note-octave 'G 9) 127))
(assert (eq? (get-pitch-from-note-octave 'C -1) 0))
(assert (eq? (get-pitch-from-note-octave 'F 5) 77))

; Åolyphony tests
(define p (pause 1/2))
(define n (note 'C 4 'piano 1/3))

(assert (eq? (polyphony-degree p) 0))
(assert (eq? (monophonic? n) #t))

; Test type
(assert (eq? (note? n) #t))
(assert (eq? (pause? p) #t))