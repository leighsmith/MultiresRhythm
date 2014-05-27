; Example of an agogic accent lengthening the last beat in a measure
; and an intensified down beat together creating a 4/4 meter.

; (IN-SYNTAX 'MIDI T)
((LAMBDA (%CONTAINER%)
   (ADD-OBJECTS
       (LIST (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.50499999999999998
                 'DURATION 0.1 :AMPLITUDE 0.2 :NOTE 'C4)
             (MAKE-OBJECT 'MIDI-NOTE 'RHYTHM 0.5 'DURATION 0.1
                 :AMPLITUDE 0.25 :NOTE 'C4))
       %CONTAINER% NIL :COPY-FIRST NIL)
   %CONTAINER%)
 (MAKE-OBJECT '(THREAD DREAD) 'ID 'DREAD))