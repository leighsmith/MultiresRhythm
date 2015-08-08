;;; -*- Lisp -*-
;;; A database of musical rhythms. 
;;; My own examples coded as Common Music programs demonstrating
;;; agogic accentuation.  Leigh Smith 4/7/95 
;;;

;;; short_corr.text Correction, 
;;; :agogic-percentage 0.2 shorten no-of-beats 3 correction
(generator short-corr rhythm-onset (note 'c4)
	   (vars (beat-ioi (rhythm 'e 80.0))
		 (which-beat 4)
                 (how-much 0.07)
		 (item-1 (final-shorten-and-correct 'four-four beat-ioi
						    which-beat how-much))
		 (item-2 (final-shorten-and-correct 'three-four beat-ioi
						    3 how-much))
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream
		  (items
		   (items #@four-four for 3)
		   (items #@three-four for 5)
		   (items #@four-four for 3)))
 		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream :kill t))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))

;;; lengthen_corr.text Correction, 
;;; :agogic-percentage 0.2 shorten no-of-beats 3 correction
(generator lengthen-corr rhythm-onset
	   (note 'c4)
	   (vars (beat-ioi (rhythm 'e 80.0))
		 (which-beat 4)
                 (how-much 0.02)
		 (item-1 (final-lengthen-and-correct
			  'four-four beat-ioi which-beat how-much
			  :measure-jitter 0.5 :beat-jitter 0.001))
		 (item-2 (final-lengthen-and-correct
			  'three-four beat-ioi 3 how-much
			  :measure-jitter 0.5 :beat-jitter 0.001))
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream
		  (items
		   (items #@four-four for 3)
		   (items #@three-four for 5)
		   (items #@four-four for 6)))
 		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream :kill t))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))


;;; try two opposite timing displacements
(generator shorten-and-lengthen-corr rhythm-onset (note 'c4)
	   (vars (beat-ioi (rhythm 'e 80.0))
		 (which-beat 4)
                 (how-much 0.01)
		 (item-1 (final-shorten-and-correct 'four-four beat-ioi
						    which-beat how-much))
		 (item-2 (final-shorten-and-correct 'three-four beat-ioi
						    3 how-much))
		 (item-3 (final-lengthen-and-correct 'four-four-long beat-ioi
						     which-beat how-much))
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream
		  (items
		   (items #@four-four for 3)
		   (items #@three-four for 5)
		   (items #@four-four for 3)
		   (items #@four-four-long for 5)))
		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream :kill t))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))


;;;  short_nocorr.text No correction, downbeat
;;; :agogic-percentage 0.2 shorten no-of-beats 3 no-correction
(generator short-nocorr rhythm-onset (length 32 note 'c4)
	   (vars (beat-ioi (rhythm 'e 85.7))
		 (which-beat 3)
                 (how-much 0.07)
		 (amplitude-stream (accented-downbeat-amplitudes which-beat))
		 (rhythm-stream (final-shorten beat-ioi which-beat how-much))
		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))


;;; short_nocorr_iso.text No correction, no intensity accent
;;; :agogic-percentage 0.2 shorten no-of-beats 3 correction
(generator short-nocorr-iso rhythm-onset (length 32 note 'c4)
	   (vars (beat-ioi (rhythm 'e 85.7))
		 (which-beat 3)
                 (how-much 0.07)
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream (final-shorten beat-ioi which-beat how-much))
		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))


;;; agogic_partial_corr.text Correction, 
;;; We can't simply correct for the agogic timing, as it just makes
;;; the beat seem late, instead, we correct by half the displacement.
;;; This produces a pause which is rather confusing for listeners as
;;; it lacks any longer duration grouping structure.
(generator agogic-part-corr rhythm-onset
	   (note 'c4 duration 0.050)
	   (vars (beat-ioi (rhythm 'e 80.0))
		 (which-beat 4)
                 (how-much 0.01)	;at 0.02 things are out of time.
		 (item-1 (final-lengthen-and-correct
			  'four-four beat-ioi which-beat how-much
			  :correct-ratio 0.5))
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream
		  (items
		   (items #@four-four for 15)))
 		 (duration-stream (fixed-duration beat-ioi which-beat)))
   	   (setf rhythm (item rhythm-stream :kill t))
           (setf amplitude (item amplitude-stream))
;	   (setf duration (item duration-stream))
)

;;; todo, implement a low frequency ritardando.
;;; agogic_partial_corr.text Correction, 
;;; We can't simply correct for the agogic timing, as it just makes
;;; the beat seem late, instead, we correct by half the displacement.
;;; This produces a pause which is rather confusing for listeners as
;;; it lacks any longer duration grouping structure.
(generator agogic-partial-correction-ritard2 rhythm-tone
	   (note 'c4 duration 0.050)
	   (vars (which-beat 4)
                 (how-much 0.01)	;at 0.02 things are out of time.
		 (item-1 (final-lengthen-and-correct
			  'four-four 
			  (rhythms e tempo ritard-accelerate-tempo-curve)
			  which-beat how-much
			  :correct-ratio 0.5))
		 (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream
		  (items
		   (items #@four-four for 15))))
   	   (setf rhythm (item rhythm-stream :kill t))
           (setf amplitude (item amplitude-stream)))

;;; Agogic action is dependent on tempo change.
;;; Need to create an anacrusis to avoid the possibility of using
;;; short term memory to create a grouping. 
;;; TODO need to correct.
(generator agogic-part-corr-ritard-dep rhythm-bowed-tone
	   (length 72 note 'c4)
	   (vars (which-beat 4)
                 (how-much 0.02)	;0.01 at 0.02 things are out of time.
		 ;; create a lengthened beat by converting it back to
		 ;; a division of a whole note at the initial tempo,
		 ;; at this time hacked to be the same as the first
		 ;; entry in ritard-accelerate-tempo-curve.
		 (agogic-eighths (list 8.0 8.0 8.0 
			(- 8.0 (/ (* 8.0 how-much) (rhythm 8.0 85.7)))))
		 (phrase (accented-phrase
			  (make-item-stream 'rhythms 'cycle agogic-eighths 
					    :tempo ritard-accelerate-tempo-curve)
;			  (accented-downbeat-amplitudes which-beat)
			  (isochronous-amplitudes)
			  (fixed-duration 0.050 which-beat))))
	   (multiple-item-bind (rh am du) (item phrase)
			       (setf rhythm rh)
			       (setf amplitude am)
			       (setf duration du)))

;(setf eighths (list 8.0 8.0 8.0 8.0))
;(setf agogic-eighths (list 8.0 8.0 8.0 
;			(- 8.0 (/ (* 8.0 how-much) (rhythm 8.0 85.7)))))
;(setf phrase (make-item-stream 'rhythms 'cycle agogic-eighths 
;			       :tempo ritard-accelerate-tempo-curve))
;(setf baseline (make-item-stream 'rhythms 'cycle eighths 
;			       :tempo ritard-accelerate-tempo-curve))
;(doitems phrase

;;; Agogic accent independent of tempo change, event shifting by a
;;; fixed quantity.
(generator agogic-part-corr-ritard-ind rhythm-onset
	   (length 72 note 'c4)
	   (vars (which-beat 4)
                 (how-much 0.02)	;at 0.02 things are out of time.

		 (phrase (accented-phrase
			  (rhythms e tempo ritard-accelerate-tempo-curve)
;			  (accented-downbeat-amplitudes which-beat)
			  (isochronous-amplitudes)
			  (fixed-duration 0.050 which-beat))))
	   (multiple-item-bind (rh am du) (item phrase)
			       (setf rhythm 
				     (final-agogic-corr rh which-beat
				;; offset the beat so we don't retain
				;; grouping due to memory of structure
							(1- count) how-much))
			       (setf amplitude am)
			       (setf duration du)))


;; I think what's happening is the period is repeating after 3 beats, but the ;; tempo pattern is repeating after 5.
(setf tempo-variation (rhythms q q. e tempo (tempo 0 80 3 85 5 80) for 30))

(setf tempo-variation (rhythms q q. e tempo (tempo from 60 to 30 in 30 update 
after) for 30))

(generator agogic-ritard midi-note (note 'c4 amplitude 0.75 duration 0.25 length 30)
(setf rhythm (item tempo-variation)))
