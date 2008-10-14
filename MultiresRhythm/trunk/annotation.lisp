;;;;
;;;; $Id$
;;;; 
;;;; Routines to annotate the given rhythm according to an identified metrical structure.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defmethod ratios-to-tactus ((rhythm-to-annotate rhythm) (rhythm-skeleton skeleton) (tactus ridge))
  "Given the rhythm, skeleton of ridges and tactus, produces the ratios to the tactus of each note."
  (let ((note-locations (onsets-in-samples rhythm-to-annotate))) ; Get sample indexes of each onset
    (loop		    ; We proceed causally thorough the rhythm, one note at a time.
       for note-time across (val note-locations)
       for ridges-for-note = (ridges-at-time rhythm-skeleton note-time)
       collect (list note-time (ridge-ratios-at-time ridges-for-note tactus note-time)))))

(defmethod annotate-rhythm ((rhythm-to-annotate rhythm) (rhythm-skeleton skeleton) (tactus ridge)
			     &key (start-from-beat 0))
  "Given the rhythm and the skeleton of ridges, produce an annotation of each note with
  respect to the tactus."
  ;; We could use Desain & Honing categorisation data to unify or prune the ratio
  ;; lists. This should be implemented as a separate function, of course.
  ;; Need also an underlying measure of ratio uncertainty due the voices-per-octave resolution.
  (ratios-to-tactus rhythm-to-annotate rhythm-skeleton tactus))

#|
(defmethod annotate-rhythm ((rhythm-to-annotate rhythm) (rhythm-skeleton skeleton) (tactus ridge)
			    &key (start-from-beat 0))
  "Given the rhythm and the skeleton of ridges, produce an annotation of each note with
  respect to the tactus."
  ;; We could use Desain & Honing categorisation data to unify or prune the ratio
  ;; lists. This should be implemented as a separate function, of course.
  ;; Need also an underlying measure of ratio uncertainty due the voices-per-octave resolution.
  (let* ((time-frequency-dimensions (.array-dimensions (scaleogram-magnitude rhythm-scaleogram)))
	 (time-in-samples (second time-frequency-dimensions))
	 
	 ;; The right way to do this is with a tight (well, two octave)
	 ;; Gaussian envelope over it.
	 ;; tactus-mag (gaussian-envelope tactus voices-per-octave)
	 ;; But just a single voice alone produces the correct oscillation, with lower amplitude:
	 (empty-magnitude (make-double-array time-frequency-dimensions))
	 (tactus-mag (dolist (tactus-ridge (if (listp tactus) tactus (list tactus)) empty-magnitude)
			     (insert-ridge tactus-ridge empty-magnitude :constant-value 1d0)))

	 ;; TODO To be completely correct the original padded output from dyadic-cwt should be used
	 ;; for input to the dyadic-icwt.
	 ;; TODO create a scalogram
	 ;; (clamped-magnitude-scaleogram (copy-instance rhythm-scaleogram))
	 ;; (setf (scaleogram-magnitude clamped-magnitude-scalogram) tactus-mag)
	 ;; (icwt clamped-magnitude-scaleogram)
	 ;; Use the modified magnitude and original phase to compute a sinusoid and it's
	 ;; Hilbert transform, from that, determine the phase of the sinusoid.
	 (foot-tap-phase (.phase (icwt tactus-mag
				       (scaleogram-phase rhythm-scaleogram)
				       (voices-per-octave rhythm-scaleogram))))

	 ;; Note the phase of the oscillating sinusoid at the beat to start tapping from.
	 (downbeat-sample (onset-time-of-note rhythm-to-annotate start-from-beat))
	 (downbeat-phase-datum (.aref foot-tap-phase downbeat-sample)))
(./ (.- (.arefs foot-tap-phase (onsets-in-samples rhythm-to-annotate)) downbeat-phase-datum)
    (* 2 pi))
    ))
|#

(defmethod annotate-rhythm ((rhythm-to-annotate rhythm))
  "Given the rhythm, produce an annotation of each note with respect to the tactus"
  (let* ((analysis (analysis-of-rhythm rhythm-to-annotate))
	 (skeleton (skeleton analysis))
	 (tactus (select-longest-lowest-tactus skeleton))) ; This is the weak link in the chain...
    (annotate-rhythm rhythm-to-annotate skeleton tactus)))

;;;; Austrian Folk song example of MTG
;;; Has an implicit anacruisis of a dotted crochet.
;;; TODO while we don't have a :tempo setting we calc it for 100bpm @ 200Hz sample rate.
(setf folk-rhythm (iois-to-rhythm "austrian-folk-rhythm"
				  '(2 2 2 4 3 1 2 2 3 1 2 2 2 2 3 1 2 2 2 2 3 1 2 2 2 2
				    ;; Bar 6 below
				    2 2 3 1 2 2 3 1 2 2 2 2 3 1 2 2 3 1 12) :shortest-ioi 120))


