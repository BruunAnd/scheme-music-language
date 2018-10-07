#lang racket
; Author: Anders Langballe Jakobsen <alja15@student.aau.dk>
; Study number: 20154059

; Utility functions from exercise solutions
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
        seconds-per-beat)
      (error ("Length must be a rational number."))))

; Get pitch given a note name and octave
(define (get-pitch note-name octave)
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
(define (seq-type? type)
  (eq? type 'sequential))

; Check if a type is a parallel composition type
(define (par-type? type)
  (eq? type 'par))

; Check if a type is a music type
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
(define seq?
  (is-element-of-type 'seq-type))

; Check if an element is a parallel composition.
(define par?
  (is-element-of-type 'par-type))

; Accessor functions
(define (get-value key element)
  (cond ((list? element)
         (let ((lookup (assoc key element)))
           (cond ((pair? lookup)
                  (pair-contents lookup))
                 (else (error("Could not look up key from association list."))))))
        (else (error("Element is not an association list.")))))

; Gets the duration from an element, given that is it either a pause or a note
(define (get-duration element)
  (cond ((or (pause? element) (note? element))
         (get-value 'duration element))
        (else (error("Cannot get duration from something that is not a note or an element.")))))
            
; Constructor functions. This collection of functions is used to create the different music elements.
; Create a pause element. Outputs an error if the duration is invalid
(define (pause! duration)
  (cond ((duration? duration) (pair-up '(type duration) (list 'pause-type duration)))
        (else error("Invalid arguments passed to pause element."))))

; Creates a note element. Performs various Check to validate that the arguments
; Two internal values (duration and pitch) are calculated before creating the note
(define (note! note-name octave instrument length)
  (let* ((pitch (get-pitch note-name octave))
         (duration (get-duration-from-length length)))
    (cond ((and (duration? duration)
                (instrument? instrument)
                (pitch? pitch))
           (pair-up '(type pitch instrument duration) (list 'note-type pitch instrument duration)))
          (else error("Invalid arguments passed to note element.")))))

(define pause (pause! 10))
(pause? pause)

(get-duration-from-length 1/4)

(define note (note! 'A 4 'guitar 8))
(note? note)
note

; notes to self
; calculate duration of par using max
