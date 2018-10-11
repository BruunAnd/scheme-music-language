#lang racket

(require "scheme-music-language.rkt")

; Notes
(display "Let's define some notes\n")
(define note-cs (note 'C# 4 'piano 1/4))
(define note-fs (note 'F# 4 'violin 1))
note-cs
note-fs

(display "Transpose example\n")
(transpose 10 note-cs)

(display "Re-instrument example\n")
(set-instrument 'guitar note-cs)

(display "Duration example\n")
(display "C# note\n")
(get-duration note-cs)
(display "F# note\n")
(get-duration note-fs)

(display "Sequence example\n")
(define a-sequence (sequence note-cs note-fs))

(display "Get duration of a sequence\n")
(get-duration a-sequence)

(display "Transpose a sequence\n")
(transpose 10 a-sequence)

(display "Scale a sequence\n")
(define a-scaled-sequence (scale 1.75 a-sequence))

(display "Duration of a scaled sequence\n")
(get-duration a-scaled-sequence)
(display "Which is unsurprisingly 1.75x the duration of a-sequence\n")

(display "Let's run the two sequences in parallel\n")
(define a-parallel (parallel a-sequence a-scaled-sequence))

(display "Take the duration of that parallel composition\n")
(get-duration a-parallel)
(display "Since the scaled version is the longest, the duration matches a-scaled-sequence\n")

(display "We can also scale, transpose and re-instrument nested music components\n")
(display "Transpose \n")
(transpose -30 a-parallel)
(display "Re-instrument\n")
(set-instrument 'guitar a-parallel)
(display "Scale\n")
(scale 3.0 a-parallel)

(display "Polyphony tests. Is a-sequence monophonic?\n")
(monophonic? a-sequence)

(display "What is the degree of polyphony of a-parallel?\n")
(polyphony-degree a-parallel)

(display "How about a new parallel where a second note starts after a pause with the same duration as the first pause?\n")
(polyphony-degree (sequence note-cs (sequence (pause 1/4) note-cs)))

(display "Canon example\n")
(define inst 'piano)
(define a-canon-sequence
  (sequence (note 'C 4 inst 1/3) (note 'C 2 inst 1/3) (note 'C 3 inst 1/3) (note 'C 4 inst 1/2) (note 'C 4 inst 1/2)
            (note 'C 4 inst 1/4) (note 'C 3 inst 1/2) (note 'C 4 inst 1/2) (note 'C 3 inst 1/3) (note 'C 4 inst 1)))

(define canon
  (parallel a-canon-sequence
            (sequence (pause 2) (transpose 10 a-canon-sequence))
            (sequence (pause 6) (transpose 20 a-canon-sequence))
            (sequence (pause 10) (transpose 30 a-canon-sequence))
            (sequence (pause 14) (transpose 40 a-canon-sequence))))

(linearize canon)

(display "What is the degree of polyphony of the canon?\n")
(polyphony-degree canon)

(display "What is the duration of the canon?\n")
(get-duration canon)