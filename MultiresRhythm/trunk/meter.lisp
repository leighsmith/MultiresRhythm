;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;; 
;;;; Meter determination.
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

;; Defines metrical structure of a rhythm, including location of each beat.
(defclass meter ()
  ((beat-times  :initarg :beat-times  :accessor beat-times)
   (hierarchy :initarg :hierarchy :accessor hierarchy) ; (representation of meter by subdivisions)
   (time-signature :initarg :time-signature :accessor time-signature)
   ;; TODO This should probably be a list of beats-per-measure over the whole piece.
   (beats-per-measure :initarg :beats-per-measure :accessor beats-per-measure :initform 4)
   (anacrusis   :initarg :anacrusis   :accessor anacrusis   :initform 0))
  (:documentation "Holds the metrical structure of a rhythm, the times of beats, and descriptions of the meter"))

;;; TODO or should I just use the anacrusis instance variable value?
(defmethod downbeats-for-anacrusis ((meter-to-analyse meter) anacrusis)
  "Returns an narray of downbeat times, from the given anacrusis"
  (let* ((beat-times (beat-times meter-to-analyse)))
    (.arefs beat-times (.iseq-inc anacrusis (- (.length beat-times) anacrusis)
				  (beats-per-measure meter-to-analyse)))))

;; Calculates the persistency of period multiples in the skeleton.
(defun meter-of-analysis-and-tactus (analysis tactus &key (meters '(3 4)))
  "Returns the evidence of harmonicity of the beat to the given beat-multiple."
  ;; Get the scales in the ridge and their relative ratios.
  ;; TODO using the first ridge in the list is a hack.
  (multiple-value-bind (tactus-scales scale-weights) (scales-and-weights-in-ridge (first tactus))
    (let* ((skeleton (skeleton analysis))
	   (vpo (voices-per-octave skeleton))
	   (persistency-profile (ridge-persistency-of analysis))
	   (beat-periods (time-support tactus-scales vpo))
	   (evidence (mapcar 
		      (lambda (beat-multiple)
			(let* ((candidate-bar-periods (.* beat-periods beat-multiple))
			       (candidate-scales (prune-outliers (.round (scale-from-period candidate-bar-periods vpo))
								 :upper-limit (1- (number-of-scales skeleton)))))
			  ;; return the weighted meter evidence
			  (if (plusp (.length candidate-scales))
			      (.sum (.* (.arefs persistency-profile candidate-scales) scale-weights))
			      0d0)))
		      meters)))
      ;; (format t "meter evidence ~a~%" evidence)
      (nth (position (apply #'max evidence) evidence) meters))))

(defun scales-of-meter (candidate-meters tactus-units beat-period vpo max-scale)
  "Given a set of candidate meters and a beat period, return the scales that the meters should have energy at"
  (declare (ignore candidate-meters))
  (loop
     for meter-multiples in '((2 4 12) (2 4 8 16) (2 6 12)) ; TODO kludged for (3/4 4/4 6/8)
     ;; for meter in candidate-meters
     ;; for meter-multiples = (make-narray (reverse (apply #'* meter))))
     ;; (apply #'* '(2 2 3) '(2 3 1))
     for units-per-beat in tactus-units
     for meter-periods = (./ (.* (make-narray meter-multiples) beat-period) units-per-beat)
     ;; clip below the lowest freq. scale.
     collect (prune-outliers (.round (scale-from-period meter-periods vpo)) :upper-limit max-scale)))

(defun meter-of-analysis (analysis &key (candidate-meters '((3 2 2) (2 2 2 2) (2 3 2)))
			  (tactus-units '(4 4 2)))
  "Determines meter using the maximum value of the cumulative sum of the scaleogram energy" 
  (let* ((scaleogram (scaleogram analysis))
	 (vpo (voices-per-octave scaleogram))
	 (persistency-profile (tempo-weighted-ridge-persistency-of analysis))
	 (most-likely-scales (most-likely-scales persistency-profile))
	 ;; TODO perhaps we should factor out the beat-period determination?
	 (beat-period (time-support (.aref most-likely-scales 0) vpo)) ; tactus
	 (scales-to-check (scales-of-meter candidate-meters tactus-units beat-period vpo (1- (number-of-scales scaleogram))))
	 (persistency-for-scales (mapcar (lambda (scales) (.arefs persistency-profile scales)) scales-to-check))
	 (evidence-for-meters (make-narray (mapcar #'mean persistency-for-scales)))
	 (likely-beat-ratios (./ (time-support most-likely-scales vpo) beat-period)))
    (plot-ridge-persistency persistency-profile scaleogram "(name analysis)")   
    (format t "beat period of maximally persistent ridge (~a) = ~a~%" (.aref most-likely-scales 0) beat-period)
    (format t "most likely scales ~a~%" (time-support most-likely-scales vpo))
    (format t "ratios of likely time periods to beat period ~a~%" likely-beat-ratios)
    (format t "scales to check ~a~%" scales-to-check)
    (format t "beat periods ~a~%" (mapcar (lambda (scales) (time-support scales vpo)) scales-to-check))
    ;; Must normalize by the number of multipliers used otherwise 4/4 will always outgun 3/4.
    (format t "persistency ~a~%mean ~a~%" persistency-for-scales evidence-for-meters)
    (nth (argmax evidence-for-meters) candidate-meters)))

(defun beat-multiple-distance (likely-beat-ratios beat-multiple)
  "Returns the distance of the likely beat ratios (ordered in decreasing likelihood) from
  multiples of the beat multiple. Smaller the value, closer the beat ratios are to the beat-multiple."
  (let ((multiple-distance (./ likely-beat-ratios beat-multiple))
	(likelihood-weighting (.rseq 1 (.length likely-beat-ratios) (.length likely-beat-ratios))))
    (format t "div by ~a ~a~%" beat-multiple multiple-distance)
    ;; TODO perhaps (.* likelihood-weighting (.abs (.- multiple--distance (.round multiple-distance)))))))
    ;; TODO but ideally use the actual weight values (or normalised) to compute the impact.
    (reduce #'* (val (.abs (.- multiple-distance (.round multiple-distance)))))))

(defun meter-of-analysis-likely (analysis &key (candidate-meters '((3 2 2) (2 2 2 2) (2 3 2)))
			  (tactus-units '(4 4 2)))
  "Determines meter using the maximum value of the cumulative sum of the scaleogram energy" 
  (let* ((scaleogram (scaleogram analysis))
	 (vpo (voices-per-octave scaleogram))
	 (persistency-profile (tempo-weighted-ridge-persistency-of analysis))
	 (most-likely-scales (most-likely-scales persistency-profile))
	 ;; TODO perhaps we should factor out the beat-period determination?
	 (beat-period (time-support (.aref most-likely-scales 0) vpo)) ; tactus
	 (likely-beat-ratios (./ (time-support most-likely-scales vpo) beat-period))
	 (nearest-multiple (.round likely-beat-ratios)))
    ;; (plot-ridge-persistency persistency-profile scaleogram "(name analysis)")   
    (format t "beat period of maximally persistent ridge (~a) = ~a~%" (.aref most-likely-scales 0) beat-period)
    (format t "most likely scales ~a~%" (time-support most-likely-scales vpo))
    (format t "ratios of likely time periods to beat period ~a~%" likely-beat-ratios)
    (format t "rounded ratios ~a~%" nearest-multiple)
    (format t "total distance from beat multiple 3 ~a~%total distance from beat multiple 2 ~a~%"
	    (beat-multiple-distance likely-beat-ratios 3.0) 
	    (beat-multiple-distance likely-beat-ratios 2.0))
    (if (< (beat-multiple-distance likely-beat-ratios 3.0) (beat-multiple-distance likely-beat-ratios 2.0))
	'(3 2 2)
	'(2 2 2 2))))

(defun meter-division (analysis tactus)
  "Returns 2 or 3 depending on whether the meter is duple or triple"
  (if (zerop (mod (meter-of-analysis-and-tactus analysis tactus) 2)) 2 3))

(defparameter *meter-names* 
  (let ((meter-names (make-hash-table :test #'equal)))
    (setf (gethash "3/4" meter-names) '(3 2 2))
    (setf (gethash "4/4" meter-names) '(2 2 2 2))
    (setf (gethash "6/8" meter-names) '(2 3 2))
    (setf (gethash "2/4" meter-names) '(2 2 2))
    (setf (gethash "3/8" meter-names) '(3 2))
    (setf (gethash "6/4" meter-names) '(3 2 2 2))
    (setf (gethash "3/2" meter-names) '(3 2 2 2))
    meter-names))

(defun meter-for-name (meter-name)
  "Returns the metrical subdivisions that the name of the meter implies"
  (gethash meter-name *meter-names*))

#|

;; Problem is, 6 times the beat-period is typically longer than the maximum scale, so
;; there will be less examples of energy. If there is, say due to a longer analysis
;; window, it is likely the energy will be more due to low frequency signals, than
;; necessarily longer periods.

;; Simply sums the magnitude modulus contributions at multiples of the candidate periods.
(defun meter-of-scale-profile (scale-profile beat-scale voices-per-octave)
  "Look for harmonicity of the beat, either duple or triple. Returns the selected beat multiple."
  (let* ((beat-period (time-support beat-scale voices-per-octave))
	 (max-period (time-support (1- (.length scale-profile)) voices-per-octave))
	 (max-multiple (floor max-period beat-period))
	 ;; Ensure there are no candidate bar scales that exceed the total scales, but
	 ;; check all multiples up to 7/8 periods.
	 ;; (beat-multiples (.iseq (min 2 max-multiple) (min 7 max-multiple)))
	 ;;TODO  must check if these exceed max-multiple.
	 (beat-multiples (make-instance 'n-fixnum-array :ival #2A((2 4) (3 6))))
	 (candidate-bar-periods (.* beat-period beat-multiples))
	 (candidate-bar-scales (.round (scale-from-period candidate-bar-periods voices-per-octave)))
	 ;; look at the scale persistency profiles at candidate-bar-scales locations
	 (bar-scale-profiles (.arefs scale-profile candidate-bar-scales))
	 (meter-evidence (.partial-sum (.transpose bar-scale-profiles)))
	 (meter-index (argmax meter-evidence)))
    (plot scale-profile nil)
    (format t "candidate bar periods ~a bar-scale-profiles ~a meter evidence ~a~%"
	    candidate-bar-periods bar-scale-profiles meter-evidence)
    (.aref beat-multiples meter-index 0)))
|#
