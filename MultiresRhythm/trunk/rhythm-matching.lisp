;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for matching between rhythm expectations and other rhythms.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2008
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :multires-rhythm)
(use-package :nlisp)

;;; calculating metric salience the Longuet-Higgins & Lee way.
(defun lh-metric-salience (meter)
  "Return list of metric weights; 0 highest level, -n lowest"
  (loop with grid-length = (apply #'* meter)
        with salience = (make-list grid-length :initial-element nil)
        initially (setf (elt salience 0) 0)
        for division in meter
        as level = -1 then (1- level)
        as interval = (/ grid-length division) then (/ interval division)
        do (loop for pos from 0 below grid-length by interval
                 do (unless (elt salience pos) (setf (elt salience pos) level)))
        finally (return salience)))

;;; Just needs the syncopation package loaded.
(defun metric-scale (meter &key (max-amp 1.0d0) (min-amp 0.12d0))
  "Derive from Longuet-Higgin's & Lee's metric salience measure (which is inverted in
   polarity) an amplitude weighting"
  (let* ((lhl-metric-salience (make-narray (lh-metric-salience meter)))
	 (max-salience (.max lhl-metric-salience))
	 (min-salience (.min lhl-metric-salience))
	 (amp-scaling (/ (- max-salience min-salience) (- max-amp min-amp))))
    ;; TODO need to scale the minimum above 0.
    (.+ 1d0 (./ (.* 1d0 lhl-metric-salience) amp-scaling))))

;;; TODO this assumes a constant tempo. This needs replacing with a function that computes
;;; the metrical structure according to the tempo estimate at each beat.
;;; Assumes 4 tatums make a beat (for bpm).
(defun metrically-scaled-rhythm (meter measures tempo &key (sample-rate 200.0d0) (beats-per-measure 4))
  "Returns a rhythm with weighted onsets matching the metrical structure"
  (let* ((metrical-weights (metric-scale meter))
	 (number-of-tatums (1+ (* (.length metrical-weights) measures))) ; +1 for the next downbeat
	 (tatums-per-beat (/ (reduce #'* meter) beats-per-measure))
	 (tatum-duration (/ 60.0d0 tempo tatums-per-beat)) ; in seconds
	 (times (nlisp::array-to-list (.* tatum-duration (.iseq 0 number-of-tatums))))
	 ;; Append the number of measures worth of weights and a final full amplitude downbeat.
	 (weights (append (loop repeat measures append (nlisp::array-to-list metrical-weights)) '(1.0d0))))
    (rhythm-of-weighted-onsets "metrical scaling" 
			       (mapcar #'list times weights)
			       ;; :duration (* tatum-duration number-of-tatums)
			       :sample-rate sample-rate)))

(defun convolve (sig1 sig2)
  "Do a Fourier domain based convolution of the two signals, which may differ in length and be non-dyadic."
  (let* ((result-length (+ (.length sig1) (.length sig2)))
	 (sufficient-pad-length (dyadic-length result-length)))
    ;; We should be dividing by sufficient-pad-length to normalise the result, but it's not worth it.
    (.subseq (.abs (ifft (.* (fft (.* (end-pad sig1 sufficient-pad-length) #C(1d0 0d0)))
		    (fft (.* (end-pad sig2 sufficient-pad-length) #C(1d0 0d0))))))
	     0 result-length)))

;; This is done using the knowledge that $r_xy(l) = x(l) * y(-l)$
(defun cross-correlation (x y)
  "Compute the cross-correlation of two signals."
  (let* ((result-length (+ (.length x) (.length y)))   ; Our convolution result is the length of the signal and filter.
	 (sufficient-pad-length (dyadic-length result-length))
	 (x-fft (fft (.* (end-pad x sufficient-pad-length) #C(1d0 0d0))))
	 (y-conj-fft (.conjugate (fft (.* (end-pad y sufficient-pad-length) #C(1d0 0d0)))))
	 (result (.realpart (ifft (.* x-fft y-conj-fft)))))
    ;; Divide by the length of the vector to renormalize the result.
    (./ (.subseq (rotate-vector result :by (- (.length y))) 0 result-length) result-length)))

;;; TODO should make the expectation precision value narrow or widen the Gaussian peak.
(defun temporal-likelihood-of-expectations (expectations temporal-discrimination)
  "Convert the expectations into a continuous Gaussian projection by convolution"
  (let* ((expectation-onsets (accumulated-confidence-of-expectations expectations))
	 (gaussian-envelope (gaussian-envelope temporal-discrimination)))
    (convolve expectation-onsets gaussian-envelope)))

;; 60mSec best temporal discrimination.
;;; But 600mSec width of sensitivity.
(defun gaussian-rhythm-envelope (rhythm &key (temporal-discrimination 0.60d0))
  "Given a rhythm, return a vector with Gaussian envelopes centered at the impulse."
  (let* ((envelope-width (round (* temporal-discrimination (sample-rate rhythm))))
	 (convolved-impulses (convolve (time-signal rhythm) (gaussian-envelope envelope-width)))
	 ;; Rotate so that the Gaussian mean is centered on each impulse.
	 (rotated-envelope (rotate-vector convolved-impulses :by (round envelope-width 2))))
    ;; Since we are interested in the convolved rhythm envelope alone, we remove the
    ;; trailing extra duration allowed for by the gaussian envelope (after rotation).
    (.subseq rotated-envelope 0 (duration-in-samples rhythm))))

(defun gaussian-rhythm-envelopes (rhythms &key (temporal-discrimination 0.60d0))
  "Given a list of rhythms, return a list of rhythm envelopes"
  (mapcar (lambda (x) (gaussian-rhythm-envelope x :temporal-discrimination temporal-discrimination)) rhythms))

;; Find the peaks in the positive lag region of the cross-correlation.  We only use
;; positive lag regions since positive lag values effectively indicate how far to
;; shift the ODF candidate *backwards* in time, thereby determining the meter
;; play region start time. Negative lag values indicate how far to shift the
;; candidate *forward* in time, but since we are nowdays only using phrases
;; within a theme, this means playing before the start of the candidate media,
;; meaning leading black or silence which would produce poor results (even if the
;; correlation score was high).
(defun cross-correlation-match (onset-detection-function metric-accent-gaussian &key (highest-correlations 10))
  "Return the sample that is the highest match between the two vectors"
  (let* ((metric-ODF-correlation (cross-correlation onset-detection-function metric-accent-gaussian))
	 ;; Since the crosscorrelation rotates the result forward by the metric-accent-gaussian
	 ;; length, the zeroth lag is at that element.
	 (zeroth-lag (.length metric-accent-gaussian))
	 (positive-lags (.subseq metric-ODF-correlation zeroth-lag))
	 ;; Find that number of highest correlation measures, returning the locations in a
	 ;; vector, in descending correlation order.
	 (selected-peaks (.subseq (highest-peaks positive-lags) 0 highest-correlations)))
    (diag-plot 'cross-correlation
      (let* ((peak-values (make-double-array (.length positive-lags))))
	(setf (.arefs peak-values selected-peaks) (.arefs positive-lags selected-peaks)) 
	(nplot (list positive-lags peak-values) nil 
	       :title (format nil "Cross Correlation of ~a" "...")
	       :styles '("lines" "points") :aspect-ratio 0.2)))
    selected-peaks))

(defun plot-correlation-matching (rhythm-1 rhythm-2 shift-by)
  "Visually verifies the cross-correlation finds the shift to align the two rhythms"
  (nplot (list rhythm-1 (.concatenate (make-double-array shift-by) rhythm-2)) nil :aspect-ratio 0.2))

;;; TODO we can probably skip the normalisation for cross-correlation-matching.
(defun match-ODF-meter (meter tempo-bpm onset-detection-rhythm)
  "Returns the sample that the meter (at the given tempo) matches the onset detection rhythm at."
  (let* ((metric-accent-gaussian (.normalise (gaussian-rhythm-envelope
					      (metrically-scaled-rhythm meter 1 tempo-bpm 
									:sample-rate (sample-rate
										      onset-detection-rhythm)))))
	 (normalised-odf (.normalise (time-signal onset-detection-rhythm)))
	 (shift-options (cross-correlation-match normalised-odf metric-accent-gaussian)))
    (diag-plot 'odf-match
      (plot-correlation-matching normalised-odf metric-accent-gaussian (.aref shift-options 0)))
    shift-options))

(defun test-correlation-matching ()
  "Visually verifies the cross-correlation finds the shift to align the two rhythms"
  (let* ((rhythm-1 (gaussian-rhythm-envelope (rhythm-of-onsets "rhythm1" (make-narray '(1.0 3.0 3.5 4.0 5.0 5.5 6.0))) 
					     :temporal-discrimination 0.3d0))
	 (rhythm-2 (gaussian-rhythm-envelope (rhythm-of-onsets "rhythm2" (make-narray '(1.5 2.0 2.5)))))
	 (shift-options (cross-correlation-match rhythm-1 rhythm-2))
	 (shift-by (.aref shift-options 0)))
    (format t "shift by ~a from options ~a~%" shift-by shift-options)
    (plot-correlation-matching rhythm-1 rhythm-2 shift-by)))

(defun match-expectations-to-candidates (expectations candidate-rhythms)
  "Returns the measure of cross correlation matches of the expectations against the
  candidate rhythms"
  (let* ((temporal-discrimination 0.060d0) ; 60mSec best temporal discrimination.
	 (match-prototype (temporal-likelihood-of-expectations expectations temporal-discrimination)))
    ;; TODO since the candidate rhythms will not be expected to change often, we can
    ;; precalculate the Gaussian envelopes for each.
    (dolist (rhythm-envelope (gaussian-rhythm-envelopes candidate-rhythms))
      (cross-correlation-match rhythm-envelope match-prototype))))
