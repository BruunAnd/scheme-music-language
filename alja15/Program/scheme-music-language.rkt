; Author: Anders Langballe Jakobsen <alja15@student.aau.dk>, study number: 20154059

(module music-language racket
  (require "pp-standard-functions-racket.scm")
  (provide monophonic? polyphony-degree note pause parallel sequence transpose set-instrument get-duration scale linearize)
  
  ; Add property to element
  (define (add-property key value element)
    (cons (cons key value) element))
    
  ; Get value from association list with specified key
  (define (get-value key assoc-list)
    (cond ((list? assoc-list)
           (let ((lookup (assoc key assoc-list)))
             (cond ((pair? lookup) (cdr lookup))
                   (else (error "Could not look up key from association list.")))))
          (else (error "Element is not an association list."))))

  ; Constructor for low level representation provided by KN
  (define (note-abs-time-with-duration abs-time channel note-number velocity duration)
    (cons 'note-abs-time-with-duration (list abs-time channel note-number velocity duration)))

  ; A higher order function which counts how many elements satisfy a predicate
  (define (count pred lst)
    (count-help pred lst 0))

  (define (count-help pred lst count)
    (cond ((null? lst) count)
          ((pred (car lst)) 
             (count-help pred (cdr lst) (+ count 1)))
          (else 
             (count-help pred (cdr lst) count))))

  ; Music theory
  (define beats-per-minute 60)
  (define time-units-per-second 960)

  ; We are dealing with a number of instruments which have a MIDI channel associated with them.
  ; This definition is an association list which pairs instruments with these channels
  (define instrument-channels '((piano . 1) (organ . 2) (guitar . 3) (violin . 4) (flute . 5) (trumpet . 6) (helicopter . 7) (telephone . 8)))

  ; We are also dealing with a number of note names. These all have some base value which
  ; is used when calculating the pitch of a note
  (define note-names '((C . 0) (C# . 1) (D . 2) (D# . 3) (E . 4) (F . 5) (F# . 6) (G . 7) (G# . 8) (A . 9) (A# . 10) (B . 11)))

  ; Check if a note name is valid
  (define (note-name? note-name)
    (let ((lookup (assoc note-name note-names)))
      (pair? lookup)))
    
  ; Check if an instrument is valid
  (define (instrument? instrument)
    (let ((lookup (assoc instrument instrument-channels)))
      (pair? lookup)))

  ; Get the channel from an instrument
  (define (get-instrument-channel instrument)
    (if (instrument? instrument)
        (cdr (assoc instrument instrument-channels))
        (error "Cannot get channel from something which is not an instrument.")))

  ; Get the base pitch from a note name
  (define (note-name-base note-name)
    (if (note-name? note-name)
        (get-value note-name note-names)
        (error "Cannot get note name base from something which is not a note name.")))

  ; Get the duration in actual time units given rational length
  ; The formula is based on a post from Music StackExchange
  ; UnitsPerSecond * Length * 60 * 4 / BPM
  (define (get-duration-from-length length)
    (if (rational? length)
        (* (* length (* 60 (/ 4 beats-per-minute))) time-units-per-second)
        (error ("Length must be a rational number."))))

  ; Get pitch given a note name and octave
  (define (get-pitch-from-note-octave note-name octave)
    (if (and (note-name? note-name) (octave? octave))
        (let ((base (note-name-base note-name)))
          (+ base (* 12 octave)))
        (error ("Invalid note name or octave provided when getting pitch."))))

  ; Various validity checks
  (define (is-int-in-range value lower upper) (and (integer? value) (>= value lower) (<= value upper)))
  (define (duration? duration) (and (integer? duration) (>= duration 0)))
  (define (pitch? value) (is-int-in-range value 0 127))
  (define (octave? value) (is-int-in-range value 1 8))

  ; Type checks
  (define (note-type? type) (eq? type 'note))
  (define (pause-type? type) (eq? type 'pause))
  (define (sequence-type? type) (eq? type 'sequential))
  (define (parallel-type? type) (eq? type 'par)) ; Check if a type is a parallel composition type
  (define (music-type? type) (or (note-type? type) (pause-type? type) (sequence-type? type) (parallel-type? type)))

  ; Get the type of an element
  ; TODO: Use in is-element-of-type
  (define (get-type element)
    (get-value 'type element))
  
  ; Helper function which returns a function for checking whether someelement is of a type. Returns #f if the element is not a list, or
  ; if assq returns something that is not a pair.
  (define (is-element-of-type type)
    (lambda (element)
      (cond ((list? element)
             (let ((lookup (assoc 'type element)))
               (cond ((pair? lookup) (eq? (cdr lookup) type))
                     (else #f))))
            (else #f))))

  ; Functins to check the type of a single music element
  (define note? (is-element-of-type 'note-type))
  (define pause? (is-element-of-type 'pause-type))
  (define sequence? (is-element-of-type 'sequence-type))
  (define parallel? (is-element-of-type 'parallel-type))
  (define (composition? element) (or (sequence? element) (parallel? element)))
  (define (music-element? element) (or (note? element) (pause? element) (sequence? element) (parallel? element)))

  ; Recursively check if a list of elements (or an element) are music elements
  (define (music-elements? elements)
    (cond ((null? elements) #t) ; The empty list of elements is a valid
          ((pair? elements) (and (music-element? (car elements)) (music-elements? (cdr elements))))
          (else #f)))

  ; Gets the instrument from a note element
  (define (get-instrument element)
    (cond ((note? element) (get-value 'instrument element))
          (else (error "Cannot get instrument from something that is not a note."))))

  ; Gets the elements from a composition element
  (define (get-elements element)
    (cond ((composition? element) (get-value 'elements element))
          (else (error "Cannot get elements from non-composition type."))))

  ; Gets the pitch from a note element
  (define (get-pitch element)
    (cond ((note? element) (get-value 'pitch element))
          (else (error "Cannot get pitch from something that is not a note."))))

  ; Gets the duration from an element
  ; For notes and pauses, we simply get the duration from the duration key
  ; For sequential compositions, we use the higher-order function reduce on the elements of the sequence, which are first mapped to a list of durations
  ; For parallel compositions, we apply the function max on the elements of a sequence
  ; Because both compositions use them, I use let to bind mapped-elements
  (define (get-duration element)
    (cond ((or (pause? element) (note? element)) (get-value 'duration element))
          ((composition? element)
           (let ((mapped-elements (map (lambda (e) (get-duration e)) (get-elements element))))
             (cond ((sequence? element) (accumulate-right + 0 mapped-elements))
                   ((parallel? element) (apply max mapped-elements)))))
          (else (error "Cannot get duration from a non-music element."))))
              
  ; Constructor functions. This collection of functions is used to create the different music elements
  ; Create a pause element. Outputs an error if the duration is invalid
  (define (pause length)
    (let ((duration (get-duration-from-length length)))
          (cond ((duration? duration) (list (cons 'type 'pause-type) (cons 'duration duration)))
          (else (error "Invalid arguments passed to pause element.")))))

  ; Creates a note element. Performs various Check to validate that the arguments
  ; Two internal values (duration and pitch) are calculated before creating the note
  (define (note note-name octave instrument length)
    (let* ((pitch (get-pitch-from-note-octave note-name octave))
           (duration (get-duration-from-length length)))
      (cond ((and (duration? duration) (instrument? instrument) (pitch? pitch))
             (list (cons 'type 'note-type) (cons 'pitch pitch) (cons 'instrument instrument) (cons 'duration duration)))
            (else (error "Invalid arguments passed to note element.")))))

  ; The approach to creating sequences and parallel compositions is similar
  ; In both cases, we wish to verify that the inner elements are all music elements
  (define (composition type elements)
    (if (music-elements? elements)
        (list (cons 'type type) (cons 'elements elements))
        (error "All elements of a composition must be music elements.")))

  ; Both accepts a variable number of parameters which are the elements of the composition
  (define (parallel . elements) (composition 'parallel-type elements))
  (define (sequence . elements) (composition 'sequence-type elements))

  ; Utility functions for music elements
  ; Update the instrument of a music element
  ; For pauses, simply return the pause element because it has no instrument
  ; For notes, return a new version of the note where the instrument property has been set to the new instrument
  ; For compositions, return new versions of the compositions where the elements of the compositions have been mapped to the re-instrument function
  (define (set-instrument instrument element)
    (cond ((note? element) (add-property 'instrument instrument element))
          ((pause? element) element)
          ((composition? element) (composition (get-type element) (map (lambda (e) (set-instrument instrument e)) (get-elements element))))
          (else (error "Cannot set instrument of a non-music element."))))

  ; Scale a music element with some factor
  ; For notes and pauses, return new elements where the duration has been multiplied
  ; For compositions, return new compositions where the elements have been mapped to scaled elements
  ; Durations are rounded sine scales can be real values
  (define (scale factor element)
    (cond ((or (note? element) (pause? element)) (add-property 'duration (exact-round (* factor (get-duration element))) element))
          ((composition? element) (composition (get-type element) (map (lambda (e) (scale factor e)) (get-elements element))))
          (else (error "Cannot scale a non-music element."))))

  ; Transpose a music element with some value
  ; No need to do anything about pauses
  ; For notes, check if the transposed pitch would exceed the legal bounds of pitches
  ; For compositions, return a new composition where its elements have been mapped to transposed elements
  (define (transpose offset element)
    (cond ((pause? element) element)
          ((note? element)
           (let ((new-pitch (+ offset (get-pitch element))))
             (if (pitch? new-pitch) (add-property 'pitch new-pitch element) (error "Transposition would result in an illegal pitch."))))
           ((composition? element) (composition (get-type element) (map (lambda (e) (transpose offset e)) (get-elements element))))
           (else (error "Cannot transpose a non-music element."))))

  ; Linearize a representation, starting at offset 0
  ; I sort it to get a nice output and because it's needed by my degree of polyphony calculator
  (define (linearize element) (linearize-helper element 0))

  ; In our base case we have a note and we use the constructor provided by KN to make an absolute note
  (define (linearize-note element offset)
    (list (note-abs-time-with-duration offset (get-instrument-channel (get-instrument element)) (get-pitch element) 80 (get-duration element))))

  ; When we linearize the tail of the sequence, we must add the length of the head
  (define (linearize-sequence elements offset)
    (if (null? elements) '()
        (let ((head (car elements)))
          (append (linearize-helper head offset) (linearize-sequence (cdr elements) (+ (get-duration head) offset))))))

  ; When we linearize the tail of a parallel composition, we use the same offset as the initial one in the tail
  (define (linearize-parallel elements offset)
    (if (null? elements) '()
        (let ((head (car elements)))
          (append (linearize-helper head offset) (linearize-parallel (cdr elements) offset)))))

  ; Note that the composition helpers handle updating offsets. Pauses "return" nothing, they serve only to update the offsets
  (define (linearize-helper element offset)
    (cond ((note? element) (linearize-note element offset))
          ((sequence? element) (linearize-sequence (get-elements element) offset))
          ((parallel? element) (linearize-parallel (get-elements element) offset))
          ((pause? element) '())))

  ; My method for calculating degree of polyphony requires that the linearized list is sorted by end time
  (define abs-note-start cadr)
  (define abs-note-duration (lambda (e) (cadr (cddddr e))))
  (define (sort-by-end list)
    (sort list < #:key (lambda (abs-note) (+ (abs-note-duration abs-note) (abs-note-start abs-note)))))

  ; Calculate degree of polyphony. I re-use the linearize function in order to get the start time and duration of notes
  (define (polyphony-degree element)
    (if (music-element? element)
      (let ((linearized (sort-by-end (linearize element))))
        (apply max (polyphony-helper linearized))) ; Since polyphony-helper returns a list of polyphonies, we need to apply max to get the highest degree
      (error "Cannot determine degree of polyphony for a non-music element.")))

  ; Higher order function for determining overlap with some other element
  (define (overlap? element)
    (let* ((own-start (abs-note-start element))
           (own-end (+ (abs-note-duration element) own-start)))
      (lambda (other) (let* ((other-start (abs-note-start other))
                             (other-end (+ (abs-note-duration other) other-start))) ; Do we need this?
                        (and (>= other-start own-start) (< other-start own-end))))))

  ; The basic principle is that for each element (head), we check if the remainder of elements overlap with this element. NOT an efficient solution, but it does the job
  (define (polyphony-helper elements)
    (if (null? elements) (list 0) ; If the list is empty, the degree of polyphony is 0
        (let* ((head (car elements))
               (remainder (cdr elements)))
          (append (polyphony-helper remainder)
                  (list (count (overlap? head) elements)))))) ; Overlap checking includes checking whether a note overlaps with itself

  ; Having implemeneted degree of polyphony, determining whether a music element is monophonic is trivial
  (define (monophonic? element)
    (if (music-element? element) (eq? (polyphony-degree element) 1)
        (error "Cannot determine whether a non-music element is monophonic.")))
)