;;; -*- Lisp -*-
#|
Personal Empirical findings from experimentation with CM and MIDI (so
these figures are probably not strictly accurate, they would be
over-estimates due to inaccuracies in playing MIDI on Unix).

BPM    ms   Hz
 33.3 1800  0.55  - Unable to be expected and clapped along to accurately
 75    800  1.25  - Seiferts Lower Limit of metrical cognisance. 
120    500  2     - Seiferts Meter Perception threshold, below this,
		    perceptual rather than cognitive or schematic rhythm
		    induction occurs. Above this, knowledge-dependent/
		    culture-dependent/schema-based control occurs.
300    200  5     - Handel's upper limit of metrical perception
361    166  6     - Freund's lower bound on fastest musical motor
		    movements (trills)
600    100 10     - Seems beyond limits of human performance, a fast
                    drum machine beat (using a high-hat sound)
750     80 12.5   - Freund's upper bound on fastest musical motor
		    movements (trills)
857     70 14.28  - lost musical meaning, streaming begins to occur
1200    50 20     - streaming has occured.

|#


(in-tempo 60)

(defun beat-frequency (msec) (/ 1 msec))
(defun bpm (msec) (/ 60 msec))

(algorithm isochronos-drums midi-note
	   (length 40 duration 0.01 amplitude 0.4)
	   (setf note 'e3)		;e3 is EPS-16+ 'Rock-Drums' Hi-Hat
	   (setf channel 1)		;1 is the drums channel
	   (setf rhythm 0.5))		;meter threshold

;; (gen-iso drums (0.5 0.1 0.07 0.05))
