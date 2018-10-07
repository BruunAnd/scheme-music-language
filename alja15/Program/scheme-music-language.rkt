#lang racket
; Author: Anders Langballe Jakobsen <alja15@student.aau.dk>
; Study number: 20154059

; Useful association list functions
; Add property to element
(define (add-property key value element)
  (cons (cons key value) element))

; Get value from association list with specified key
(define (get-value key element)
  (cond ((list? element)
         (let ((lookup (assoc key element)))
           (cond ((pair? lookup)
                  (pair-contents lookup))
                 (else (error("Could not look up key from association list."))))))
        (else (error("Element is not an association list.")))))

; Written by Kurt Nørmark (TODO: Ask if allowed)
(define (pair-up key-list value-list)
  (if (or (null? key-list) (null? value-list))
      '()
      (cons
       (cons (car key-list) (car value-list))
       (pair-up (cdr key-list) (cdr value-list)))))

; Additional definitions to increase readability
(define pair-contents cdr)

; Music theory
(define beats-per-minute 100)
(define time-units-per-second 960)

; We are dealing with a number of instruments which have a MIDI channel associated with them.
; This definition is an association list which pairs instruments with these channels
(define instrument-channels '((piano . 1)
                              (organ . 2)
                              (guitar . 3)
                              (violin . 4)
                              (flute . 5)
                              (trumpet . 6)
                              (helicopter . 7)
                              (telephone . 8)))

; We are also dealing with a number of note names. These all have some base value which
; is used when calculating the pitch of a note
(define note-names '((C . 0)
                     (C# . 1)
                     (D . 2)
                     (D# . 3)
                     (E . 4)
                     (F . 5)
                     (F# . 6)
                     (G . 7)
                     (G# . 8)
                     (A . 9)
                     (A# . 10)
                     (B . 11)))

; Check if a note name is valid
(define (note-name? note-name)
  (let ((lookup (assoc note-name note-names)))
    (pair? lookup)))
  
; Check if an instrument is valid
(define (instrument? instrument)
  (let ((lookup (assoc instrument instrument-channels)))
    (pair? lookup)))

; Get the channel from an instrument
(define (instrument-channel instrument)
  (if (instrument? instrument)
      (pair-contents (assoc instrument instrument-channels))
      (error("Cannot get channel from something which is not an instrument."))))

; Get the base pitch from a note name
(define (note-name-base note-name)
  (if (note-name? note-name)
      (get-value note-name note-names)
      (error ("Cannot get note name base from something which is not a note name."))))

; Get the duration in actual time units given rational length
(define (get-duration-from-length length)
  (if (rational? length)
      (let ((seconds-per-beat (/ 60 beats-per-minute)))
        100) ; TODO: Get actual duration
      (error ("Length must be a rational number."))))

; Get pitch given a note name and octave
(define (get-pitch-from-note-octave note-name octave)
  (if (and (note-name? note-name)
           (octave? octave))
      (let ((base (note-name-base note-name)))
        (+ base (* 12 octave)))
      (error ("Invalid note name or octave provided when getting pitch."))))

; An inclusive check for whether an integer is in some range
(define (is-int-in-range value lower upper)
  (and (integer? value)
       (>= value lower)
       (<= value upper)))

; Check if a duration is valid
(define (duration? duration)
  (and (integer? duration)
       (>= duration 0)))

; Check if a pitch value is in the valid range
(define (pitch? value)
  (is-int-in-range value 0 127))

; Check if an octave value is in the valid range
(define (octave? value)
  (is-int-in-range value 1 8))

; Check if a type is a note type
(define (note-type? type)
  (eq? type 'note))

; Check if a type is a pause type
(define (pause-type? type)
  (eq? type 'pause))

; Check if a type is a sequence type
(define (sequence-type? type)
  (eq? type 'sequential))

; Check if a type is a parallel composition type
(define (parallel-type? type)
  (eq? type 'par))

; Check if a type is a music type
(define (music-type? type)
  (or (note-type? type)
      (pause-type? type)
      (sequence-type? type)
      (parallel-type? type)))

; All music elements in my music language are association lists
; As such, I can check if some list is a music element by checking its type.

; Helper function which returns a function for checking whether some
; element is of a type. Returns #f if the element is not a list, or
; if assq returns something that is not a pair.
; TODO: Use some form of has-key and get-value-value instead
(define (is-element-of-type type)
  (lambda (element)
    (cond ((list? element)
           (let ((lookup (assoc 'type element)))
             (cond ((pair? lookup)
                    (eq? (pair-contents lookup) type))
                   (else #f))))
          (else #f))))

; Check if an element is a note.
(define note?
  (is-element-of-type 'note-type))

; Check if an element is a pause.
(define pause?
  (is-element-of-type 'pause-type))

; Check if an element is a sequence.
(define sequence?
  (is-element-of-type 'sequence-type))

; Check if an element is a parallel composition
(define parallel?
  (is-element-of-type 'parallel-type))

; Check if an element is a composition type
(define (composition? element)
  (or (sequence? element)
      (parallel? element)))

; Check if a single element is a music element
(define (music-element? element)
  (or (note? element)
      (pause? element)
      (sequence? element)
      (parallel? element)))

; Recursively check if a list of elements (or an element) are music elements
(define (music-elements? elements)
  (cond ((null? elements) #t) ; The empty list of elements is a valid
        ((pair? elements) (and (music-element? (car elements))
                               (music-elements? (cdr elements))))
        (else #f)))

; Accessor functions
; Gets the elements from a composition element
(define (get-elements element)
  (if (composition? element)
      (get-value 'elements element)
      (error("Cannot get elements from non-composition type."))))

; Gets the pitch from a note element
(define (get-pitch element)
  (cond ((note? element) (get-value 'pitch element))
        (else (error("Cannot get pitch from something that is not a note.")))))

; Gets the duration from an element, given that is it either a pause or a note
(define (get-duration element)
  (cond ((or (pause? element) (note? element))
         (get-value 'duration element))
        (else (error("Cannot get duration from something that is not a note or an element.")))))
            
; Constructor functions. This collection of functions is used to create the different music elements
; Create a pause element. Outputs an error if the duration is invalid
(define (pause! length)
  (let ((duration (get-duration-from-length length)))
        (cond ((duration? duration)
               (pair-up '(type duration) (list 'pause-type duration)))
        (else error("Invalid arguments passed to pause element.")))))

; Creates a note element. Performs various Check to validate that the arguments
; Two internal values (duration and pitch) are calculated before creating the note
(define (note! note-name octave instrument length)
  (let* ((pitch (get-pitch-from-note-octave note-name octave))
         (duration (get-duration-from-length length)))
    (cond ((and (duration? duration)
                (instrument? instrument)
                (pitch? pitch))
           (pair-up '(type pitch instrument duration) (list 'note-type pitch instrument duration)))
          (else error("Invalid arguments passed to note element.")))))

; The approach to creating sequences and parallel compositions is similar
; In both cases, we wish to verify that the inner elements are all music elements
(define (composition! type elements)
  (if (music-elements? elements)
      (pair-up '(type elements) (list type elements))
      (error("All elements of a composition must be music elements."))))

; Creates a sequential composition element
; Accepts a variable number of parameters which are the elements of the composition
(define (sequence! . elements)
  (composition! 'sequence-type elements))

; Creates a parallel composition element
; Accepts a variable number of parameters which are the elements of the composition
(define (parallel! . elements)
  (composition! 'parallel-type elements))

; Utility functions for music elements
; Update the instrument of a music element
; For pauses, simply return the pause element because it has no instrument
; For notes, return a new version of the note where the instrument property has been set to the new instrument
; For compositions, return new versions of the compositions where the elements of the compositions have been mapped to the re-instrument function
(define (set-instrument instrument element)
  (cond ((note? element) (add-property 'instrument instrument element))
        ((pause? element) element)
        ((composition? element) (add-property 'elements (map (lambda (e) (set-instrument instrument e)) (get-elements element)) element))
        (else error("Cannot set instrument of a non-music element."))))

; Scale a music element with some factor
; For notes and pauses, return new elements where the duration has been multiplied
; For compositions, return new compositions where the elements have been mapped to scaled elements
(define (scale factor element)
  (cond ((or (note? element) (pause? element)) (add-property 'duration (* factor (get-duration element)) element))
        ((composition? element) (add-property 'members (map (lambda (e) (scale factor e)) (get-elements element)) element))
        (else error("Cannot scale a non-music element."))))

; Transpose a music element with some value
; No need to do anything about pauses
; For notes, check if the transposed pitch would exceed the legal bounds of pitches
; For compositions, return a new composition where its elements have been mapped to transposed elements
(define (transpose offset element)
  (cond ((pause? element) element)
        ((note? element)
         (let ((new-pitch (+ offset (get-pitch element))))
           (if (pitch? new-pitch)
               (add-property 'pitch new-pitch element)
               (error("Transposition would result in an illegal pitch.")))))
         ((composition? element) (add-property 'members (map (lambda (e) (transpose offset e)) (get-elements element)) element))
         (else error("Cannot transpose a non-music element."))))
           
; notes to self
; calculate duration of par using max
; re-instrument and scale may use the higher order map function

(define noteC (note! 'C# 4 'guitar 1/4))
(define noteB (note! 'B 2 'piano 3/4))
(define pause (pause! 2/4))

(define sequence (sequence! noteB pause noteC))
(define parallel (parallel! sequence sequence noteB))
(parallel? (set-instrument 'helicopter parallel))
(scale 5 noteC)
(scale 10 parallel)
(transpose 10 noteC)
(transpose 10 sequence)