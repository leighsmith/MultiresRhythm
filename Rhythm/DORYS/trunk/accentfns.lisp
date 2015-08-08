;;; -*- Lisp -*-
;;; A database of musical rhythms. 
;;; Functions to return item streams suitable for constructing accents.  
;;; Leigh Smith 4/7/95 
;;;

;;;
;;; * Amplitude *
;;;

;;; Definitions of intensity (amplitude) based accenting
;;; Should be of intensity-cue class
(defun isochronous-amplitudes ()
  (items 0.98))

;; create an amplitude item stream with the first beat louder. 
(defun accented-downbeat-amplitudes (which-beat)
  (items 0.98 (items 0.65 for (- which-beat 1))))

;;; Describes a primary downbeat with a mid period upbeat syncopation
(defun canonical-4-4-beat-amplitudes (which-beat)
  (let ((offbeat (items 0.65 for (/ (- which-beat 2) 2))))
    (items 0.98 offbeat 0.8 offbeat)))

;;;
;;; * Agogics *
;;;

;;; A repeated isochronous pattern of beats without deviation.
(defun isochronous-ioi (beat-ioi which-beat how-much)
  (items (expr beat-ioi) for which-beat))

;;; Describes a lagging of the beat prior to the downbeat, with a
;;; correction following it.
;;; Should be of an agogic-cue class, which should be a sub-class of
;;; an item-stream.
;;; we add a random jitter to the correction, so it isn't entirely
;;; periodic. The jitter parameter is a normalised measure of randomness added
(defun final-lengthen-and-correct
  (stream-name beat-ioi which-beat how-much
	       &key (measure-jitter 0.0) (beat-jitter 0.0) (correct-ratio 1.0))
  (items 
   ;; pre-correct 
; should have both measure-jitter and correct-ratio
;   (expr (- beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
   (expr (- beat-ioi (* how-much correct-ratio)))
   ;; IOIs as specified, allowing for beat-jitter
   (items (expr (+ beat-ioi (between (- beat-jitter) beat-jitter)))
	  for (- which-beat 2))
   ;; lengthen
   (expr (+ beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
   named stream-name))

;; Ideally this should take a tempo curve an apply it after the agogic modulation.
;; make-item-stream to do that.
(defun final-lengthen-and-correct-tempocurve
  (stream-name beat-ioi which-beat how-much &key (measure-jitter 0.0)
	       (beat-jitter 0.0)
	       (correct-ratio 1.0)
	       (tempo-curve))
  (make-item-stream 
   'items 'cycle
   ;; pre-correct 
   ;; should have both measure-jitter and correct-ratio
   ;; (expr (- beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
   (list (expr (- beat-ioi (* how-much correct-ratio)))
   ;; IOIs as specified, allowing for beat-jitter
	 (items (expr (+ beat-ioi (between (- beat-jitter) beat-jitter)))
		for (- which-beat 2))
   ;; lengthen
	 (expr (+ beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
	 :named stream-name)))


;;; what I want to do is be able to deform one item stream with
;;; another, that is, apply deviations to one item stream from another.


;;; from agogic+ritard
;;; Describes a lagging of the beat prior to the downbeat, with a
;;; correction following it.
;;; Should be of an agogic-cue class, which should be a sub-class of
;;; an item-stream.
;;; we add a random jitter to the correction, so it isn't entirely
;;; periodic. The jitter parameter is a normalised measure of randomness added
(defun final-lengthen-and-correct-2 (stream-name beat-ioi which-beat how-much
					       &key (measure-jitter 0.0)
					       (beat-jitter 0.0))
  (items (expr (- beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
	 (items
	  (expr (+ beat-ioi (between (- beat-jitter) beat-jitter)))
			       for (- which-beat 2))
	 (expr (+ beat-ioi (* how-much (- 1.0 (between 0.0 measure-jitter)))))
	 named stream-name))

;;; Given a nominal beat-ioi, as the current-beat'th beat, swing the
;;; accent-every'th beat by extending beat-ioi by how-much. An
;;; accompanying compensation for the agogic accent on the next beat
;;; is created. Assumes current-beat is measured from 0.
(defun final-agogic-corr (beat-ioi accent-every current-beat how-much)
  (let ((measure-index (mod current-beat accent-every)))
    (cond 
     ((<= current-beat 0) beat-ioi)
     ((zerop measure-index) (- beat-ioi how-much))
     ((eq measure-index (1- accent-every))
      (+ beat-ioi how-much))
     (t beat-ioi))))

;;; Describes a lagging of the final beat of the measure (which-beat durations)
(defun final-lengthen
  (stream-name beat-ioi which-beat how-much &key (measure-jitter 0.0) (beat-jitter 0.0))
  (items (items
	  (expr beat-ioi) for (- which-beat 1))
	 (expr (+ beat-ioi how-much))
	 named stream-name))

;;; Describes a pre-mature leading ahead of the final beat of the
;;; measure (which-beat durations) with a correction to retain strict
;;; metricality. Generates an item-stream of the provided name.
(defun final-shorten-and-correct (stream-name beat-ioi which-beat how-much)
  (items (expr (+ beat-ioi how-much))
	 (items (expr beat-ioi) for (- which-beat 2))
	 (expr (- beat-ioi how-much)) named stream-name))

;;; Describes a pre-mature leading ahead of the final beat of the
;;; measure (which-beat durations)
(defun final-shorten (beat-ioi which-beat how-much)
  (items (items (expr beat-ioi) for (- which-beat 1))
	 (expr (- beat-ioi how-much))))
;;;
;;; * Duration/Articulation *
;;;

;;; These need to access the rhythm so they can produce the duration
;;; as relative displacements with respect to the rhythm.

;;; Describes a lengthening of the duration of the final beat of the
;;; measure (which-beat events).
(defun legato-accent (beat-ioi which-beat how-much)
  (items (items (expr (* beat-ioi 0.5)) for (- which-beat 1))
	 (expr (+ (* beat-ioi 0.5) how-much))))

;;; Describes a isochronous duration which is a tenth the period of the
;;; rhythm.
(defun fixed-duration (beat-ioi which-beat)
  (items (expr (* beat-ioi 0.1))))

;;; Describes an isochronous duration which is exactly the length of
;;; the beat-ioi
(defun equal-duration (beat-ioi)
  (items (expr beat-ioi)))

;;; Describes a staccato on the second beat of the measure
(defun mazurka-accent (which-beat)
)

;;;
;;; * Pitch *
;;;

;;; A pitch based accent, higher pitched note on the downbeat
(defun high-pitched-downbeat (which-beat)
  (item (notes g4 (notes c4 for (- which-beat 1)))))
