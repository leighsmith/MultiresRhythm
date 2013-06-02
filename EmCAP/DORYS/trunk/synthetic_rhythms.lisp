;;; -*- Lisp -*-
;;; DORYS - A Database Of RhYthmic Stimuli
;;;
;;; My own examples coded as Common Music programs to act as a set
;;; of stimulus data to compare human and automatic perception
;;; performances.  Leigh Smith 4/7/95

(defmacro rhythm-instrument () 'rhythm-onset)

;;;
;;; 

;;; define a multiple item generator to lock rhythmic accenting
;;; functions together, rhythm, amplitude and duration.
(defmultiple-item accented-phrase (rhythm amplitude duration)
  (:element-parser (lambda (rh am du) (list rh am du))))

;;; Really we should define our behaviours as named defmultiple-items
(defmultiple-item unaccented-ritard (rhythm amplitude duration)
  (:element-parser (lambda (rh am du) (list rh am du))))

;;; Meters formed from Intensity or duration accenting.
;;; Demonstrating changing from one meter to another, with isochronous IOI's

;;; A generator using multiple-item-streams
;;; However, we should tuck most of this behind a macro 
(generator intensity34-to-44 rhythm-onset (length 62 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(phrase (accented-phrase
		 (isochronous-ioi beat-ioi which-beat how-much)
		 (items
		  (items 0.95 0.75 0.75      for (* 3 4) counting values)
		  (items 0.95 0.75 0.75 0.75 for (* 4 5))
		  (items 0.95 0.75 0.75      for (* 3 10)))
;;;			  (items
;			   (items (expr (accented-downbeat-amplitudes 3)) for 4)
;			   (items (expr (accented-downbeat-amplitudes 4)) for 3)
;			   (items (expr (accented-downbeat-amplitudes 3)) for 4))
		 (equal-duration beat-ioi)
;;		 (legato-accent beat-ioi which-beat how-much)
)))
  (multiple-item-bind (rh am du) (item phrase)
    (setf rhythm rh)
    (setf amplitude am)
    (setf duration du))) 

;;; Same 3/4 to 4/4 transition as above, but with much longer 3/4 periods.
;;; A generator using multiple-item-streams
;;; However, we should tuck most of this behind a macro 
(generator long-intensity34-to-44 rhythm-onset (length 260 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(phrase (accented-phrase
		 (isochronous-ioi beat-ioi which-beat how-much)
		 (items
		  (items 0.95 0.75 0.75      for (* 3 30) counting values)
		  (items 0.95 0.75 0.75 0.75 for (* 4 5))
		  (items 0.95 0.75 0.75      for (* 3 50)))
;;;			  (items
;;;			   (items (expr (accented-downbeat-amplitudes 3)) for 4)
;;;			   (items (expr (accented-downbeat-amplitudes 4)) for 3)
;;;			   (items (expr (accented-downbeat-amplitudes 3)) for 4))
		 (legato-accent beat-ioi which-beat how-much))))
  (multiple-item-bind (rh am du) (item phrase)
    (setf rhythm rh)
    (setf amplitude am)
    (setf duration du))) 

;;; intensity34_to_44.text
;;; Compare my method of amplitude-stream generation with duration44-to-34
;;; Currently I get the result I'm after, but the 44 period is longer
;;; than I declare.
(generator old-intensity34-to-44 rhythm-onset (length 32 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(amplitude-stream (items
			   (items (accented-downbeat-amplitudes 3) for 18)
			   (items (accented-downbeat-amplitudes 4) for 16)
			   (items (accented-downbeat-amplitudes 3) for 16)))
	(rhythm-stream (isochronous-ioi beat-ioi which-beat how-much))
	(duration-stream (fixed-duration beat-ioi which-beat)))
  (setf rhythm (item rhythm-stream))
  (setf amplitude (item amplitude-stream))
  (setf duration (item duration-stream)))


;;; A ritard then accelerate (at different rates) tempo curve which
;;; demonstrates behaviour when deviating from strict periodicity 
(setf ritard-accelerate-tempo-curve
      (tempo 16 85.7 30 42.0 44 42.0 52 85.7 72 85.7 pulse 'e))

;;; An approximation to sinusoidal rhythm modulation - slower then
;;; faster, offset to avoid window symmetry effects.
;;; The beat number is the number of crochets.
(setf sinusoidal-tempo-curve
      (tempo 10 100 22 65 42 135 54 100 72 100 pulse 'q))

;;; A ritard with equal amplitudes for each beat.
(generator ritard rhythm-onset (length 72 note 'c4)
  (vars (which-beat 4)
	(phrase (accented-phrase
		 (rhythms e tempo ritard-accelerate-tempo-curve)
		 (isochronous-amplitudes)
		 (fixed-duration (rhythm 'e 85.7) which-beat))))
  (multiple-item-bind (rh am du) (item phrase)
    (setf rhythm rh)
    (setf amplitude am)
    (setf duration du)))

;;; A ritard with the downbeat of every 4 beats accented
(generator ritard44 rhythm-onset (length 72 note 'c4)
  (vars (which-beat 4)
	(phrase (accented-phrase
		 (rhythms e tempo ritard-accelerate-tempo-curve)
		 (accented-downbeat-amplitudes which-beat)
		 (fixed-duration (rhythm 'e 85.7) which-beat))))
  (multiple-item-bind (rh am du) (item phrase)
    (setf rhythm rh)
    (setf amplitude am)
    (setf duration du)))

;;; duration44_to_34.text
;;; create an accent by duration and change the meter from 4/4 to 3/4
;;; This requires all the item streams to be declared in a single
;;; macro. This misses the opportunity to re-use item streams.
(generator duration44-to-34 rhythm-energy-square (length 48 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(beat-dur (* beat-ioi 0.5))
	(which-beat 3)
	(amplitude-stream (isochronous-amplitudes))
	(rhythm-stream (isochronous-ioi beat-ioi which-beat nil))
	(duration-stream
	 (items
	  (items (items 0.24 (items (expr beat-dur) for 3)) for 4)
	  (items (items 0.24 (items (expr beat-dur) for 2)) for 4)
	  (items (items 0.24 (items (expr beat-dur) for 3)) for 5))))
  (setf rhythm (item rhythm-stream))
  (setf amplitude (item amplitude-stream))
  (setf duration (item duration-stream)))


;;		  (items
;;		   (items (items (items beat-dur for 2) 0.2) 
;;				 (expr (+ (expr beat-dur) 0.07))) for 4)

;;; A rhythm mach band? Probably not, but I need some name for the
;;; damn thing. Here we do some ritards over a couple of ranges before
;;; speeding up again.
(generator mach-band rhythm-onset (length 64 note 'c4)
  (vars (beat-ioi (rhythm 'q 80.0))
	(which-beat 4)
	(phrase (accented-phrase
		 (rhythms e tempo
			  (tempo 16 160.0 32 80.0 48 80.0
				 64 60 80 pulse 'q))
		 (isochronous-amplitudes)
		 (fixed-duration beat-ioi which-beat))))
  (multiple-item-bind (rh am du) (item phrase)
    (setf rhythm rh)
    (setf amplitude am)
    (setf duration du)))

(generator accent-phrase rhythm-onset
	   (length 64 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(amplitude-stream (isochronous-amplitudes))
	(rhythm-stream (isochronous-ioi beat-ioi which-beat how-much))
	(duration-stream (legato-accent beat-ioi which-beat how-much)))
  (setf rhythm (item rhythm-stream))
  (setf amplitude (item amplitude-stream))
  (setf duration (item duration-stream)))

(generator meter-change rhythm-onset
	   (length 64 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(amplitude-stream (items
			   (items 0.95 (items 0.75 for 2) for 12)
			   (items 0.95 (items 0.75 for 3) for 12)
			   (items 0.95 (items 0.75 for 2) for 30)))
	(rhythm-stream (isochronous-ioi beat-ioi which-beat how-much))
	(duration-stream (fixed-duration beat-ioi which-beat)))
  (setf rhythm (item rhythm-stream))
  (setf amplitude (item amplitude-stream))
  (setf duration (item duration-stream)))

;;; iso_duration34.text
;;;
(generator iso-duration34 rhythm-energy-square (length 32 note 'c4)
  (vars (beat-ioi (rhythm 'e 85.7))
	(which-beat 3)
	(how-much 0.07)
	(amplitude-stream (isochronous-amplitudes))
	(rhythm-stream (isochronous-ioi beat-ioi which-beat how-much))
	(duration-stream (legato-accent beat-ioi which-beat how-much)
			 ))
  (setf rhythm (item rhythm-stream))
  (setf amplitude (item amplitude-stream))
  (setf duration (item duration-stream)))



;;; A complex (yet groupable) sequence which is ritarding to the point
;;; of lost meter.
(generator complex-ritard rhythm-onset
	   (note 'c4 amplitude 0.75 duration 0.25 length 30)
  (setf rhythm (item (rhythms e q q. tempo 
			      (tempo from 60 to 30 in 30 update after) for 30))))

;;; For Rameri, a random rhythm generator, to spot any
;;; non-orthogonality in our analysis. We specify a random IOI range
;;; which can exceed the maximum period of our wavelet
;;; (2048samples/200Hz) so we don't constrain data to lie within a
;;; rhythmic upper bound (thereby creating an analysable periodicity).
(generator random-rhythms rhythm-onset (length 20 amplitude 0.95 note 'c4)
  (setf rhythm (random 10.24))
  (setf duration (/ rhythm 3)))		; meaningless with
					; rhythm-onset, but we may
					; want to play it someday...
		       
;;; Pulse thread, we should generate a 1000Hz sound file with this to
;;; ensure the IOI's are 256 samples. This will allow us to see the
;;; harmonics with respect to a coincident sampling rate.
(thread pulse ()
  (dotimes (i 16)
    (object rhythm-onset note 'c4 rhythm 0.256 duration 0.256
	    amplitude 0.95))) 

;;; Generate an accented beat every 4th beat of an otherwise isochronous pulse.
(generator accented-pulse rhythm-onset 
	   (length 32 rhythm 0.256 note 'c4 duration 0.256)
  (vars (amplitude-stream (accented-downbeat-amplitudes 4)))
  (setf amplitude (item amplitude-stream)))

;;;
;;; Flamenco Rhythm from CD program notes by Paco Pena.
;;;
;;; accents on the (3 6 8 10 12) beats of a 12 beat measure
;;; assuming 12/8 meter
(generator flamenco-canas rhythm-onset
	   (length 20 amplitude 0.75 duration 0.158)
  (setf note (item (notes rest (notes a4 for 5))))
  (setf rhythm (item (rhythms q q. q q q e tempo 120))))

;;; seven-eight, (with the answer to the meaning-of-life embedded in
;;; it).
(generator seven-eight-pattern rhythm-onset
           (length 72 duration 0.158 note 'c4)
  (vars (seven-eight-accent 
         (items 0.98 0.6 0.98 0.6 0.98 0.6 0.6)))
  (setf amplitude (item seven-eight-accent))
  (setf rhythm (item (rhythms e tempo ritard-accelerate-tempo-curve))))

;;; Seven/Eight rhythm (only the accented beats) undergoing a
;;; ritard-accelerate-ritard sinusoidal tempo curve.
(generator seven-eight-sinusoid rhythm-onset
	   (length 64 duration 0.158 note 'c4)
  (setf amplitude 0.7)
  (setf rhythm (item (rhythms q q q. tempo sinusoidal-tempo-curve))))

;;;
;;; anapest rhythm with rubato
(generator anapest-with-rubato rhythm-tone  ; rhythm-onset
	   (length 64 note 'c4)
	   (vars (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream (rhythms e e q tempo ritard-accelerate-tempo-curve))
		 (duration-stream (fixed-duration 0.25 3)))
   	   (setf rhythm (item rhythm-stream))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))

