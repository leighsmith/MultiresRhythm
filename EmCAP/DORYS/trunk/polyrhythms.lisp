;;; -*- Lisp -*-
;;; Polyrhythms. Gives the simultaneous number of beats in a given time
;;; period to be performed in a dissonant rhythmic lines.

;;; (3 4) is 3 beats and 4 beats performed isochronously in x time
;;; period repetively.

;;
;(polyrhythm '(3 4))

;; 7 is preferred
;(polyrhythm '(2 3 7))

;; 2 is preferred
;(polyrhythm '(2 5 7))

;;
;(polyrhythm '(3 4 5))

;probably sprout a merge of the lines
;(sprout (merge 

;;; African patterns Handel p405
;;; Number of IOI beats forming a repeating pattern.
(setf handel-african-pattern-1 '(3 3 2))
(setf handel-african-pattern-2 '(2 2 2 3))
(setf handel-african-pattern-3 '(2 2 2 3 3))
(setf handel-african-pattern-4 '(2 2 3 2 3))
(setf handel-african-pattern-5 '(2 3 2 2 3))
(setf handel-african-pattern-6 '(2 2 2 3 2 2 3))
(setf handel-african-pattern-7 '(3 3 4 2 4))

;;; African Drum patterns, Handel p403
;;;*        1  *            ; downbeats
;1234567890123456789
;X-X--X-X-X--X-X--X-X-X-- ; 12 units in 2secs or less, claps on 1,3,6,8,10
;X-X-X--X-X--X-X-X--X-X--

#|
;;; Seifert, Olk and Schneider 1995 seifert:theory African polyrhythms
;;; Figure 1
seifert-figure-1

triangle
cowbell
bassdrum
highdrum
agogos
timbales
congas
bongos
lowdrums
claves
wood-block
handclap
marimba

;;; Figure 2
seifert-figure-2

timbale
handclap
rimshot
hihat-closed
cabasa
triangle
tom-tom-low
tom-tom-mid
snare
shaker
bell-ride
maracas
cowbell
claves
conga-hi
conga-low


;;; Figure 3
seifert-figure-3

triangle-open
triangle-muted
agogo-hi
agogo-low
cowbell
conga-mute
conga-hi
conga-low
native-drum-2
native-drum-3
|#


;;; 5 in space of 4
((LAMBDA (%CONTAINER%)
    (ADD-OBJECTS
      (LIST
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'C4 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          1.0
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'D4 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          1.0
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'E4 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          1.0
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'F4 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          1.0
      ) )
      %CONTAINER% NIL :COPY-FIRST NIL
    )
    %CONTAINER%
 )
  (MAKE-OBJECT '(THREAD FOO) :ID 'FOO)
)
((LAMBDA (%CONTAINER%)
    (ADD-OBJECTS
      (LIST
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'C5 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          0.8
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'D5 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          0.8
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'E5 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          0.8
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'F5 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          0.8
        )
        (MAKE-OBJECT 'MIDI-NOTE 'NOTE 'C5 'AMPLITUDE 0.8 :DURATION 0.5 :RHYTHM
          0.8
      ) )
      %CONTAINER% NIL :COPY-FIRST NIL
    )
    %CONTAINER%
 )
  (MAKE-OBJECT '(THREAD FOO2) :ID 'FOO2)
)
