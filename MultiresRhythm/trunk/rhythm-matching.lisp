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

(in-package :multires-rhythm)
(use-package :nlisp)

;;; Just needs the syncopation package loaded.
(defun metric-scale (meter &key (max-amp 1.0d0) (min-amp 0.12d0))
  "Derive from Longuet-Higgin's & Lee's metric salience measure (which is inverted in
   polarity) an amplitude weighting"
  (let* ((lhl-metric-salience (make-narray (syncopation::lh-metric-salience meter)))
	 (max-salience (.max lhl-metric-salience))
	 (min-salience (.min lhl-metric-salience))
	 (amp-scaling (/ (- max-salience min-salience) (- max-amp min-amp))))
    ;; TODO need to scale the minimum above 0.
    (.+ 1d0 (./ (.* 1d0 lhl-metric-salience) amp-scaling))))

;;; TODO this assumes a constant tempo. This needs replacing with a function that computes
;;; the metrical structure according to the tempo estimate at each beat.
(defun metrically-scaled-rhythm (meter measures tempo)
  "Returns a rhythm with weighted onsets matching the metrical structure"
  (let* ((metrical-weights (metric-scale meter))
	 ;; (number-of-tatums (* (1+ (reduce #'* meter)) measures))
	 (number-of-tatums (* (.length metrical-weights) measures))
	 (tatum-duration (/ 60.0d0 tempo 4.0)) ; in seconds, assumes 4 tatums make a beat (for bpm).
	 (times (nlisp::array-to-list (.* tatum-duration (.iseq 0 number-of-tatums)))))
    (rhythm-of-weighted-onsets "metrical scaling" (mapcar #'list times (nlisp::array-to-list metrical-weights)))))

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
    (.subseq (rotate-vector result :by (- (.length y))) 0 result-length)))

    ;; Divide by the length of the vector to renormalize the result.
    ;; (./ result result-length)))

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
	 (rotated-envelope (rotate-vector convolved-impulses :by (/ envelope-width 2))))
    ;; Since we are interested in the convolved rhythm envelope alone, we remove the
    ;; trailing extra duration allowed for by the gaussian envelope (after rotation).
    (.subseq rotated-envelope 0 (duration-in-samples rhythm))))

(defun gaussian-rhythm-envelopes (rhythms)
  "Given a list of rhythms, return a list of rhythm envelopes"
  (mapcar #'gaussian-rhythm-envelope rhythms))

(defun cross-correlation-match (meter measure-duration onset-detection-function)
  (let* ((metric-accent-gaussian (gaussian-rhythm-envelope (metrically-scaled-rhythm meter measure-duration)))
	 (acceptable-correlation-measures 1)
	 (metric-ODF-correlation (cross-correlation onset-detection-function metric-accent-gaussian))
	 ;; Since the crosscorrelation rotates the result forward by the metric-accent-gaussian
	 ;; length, the zeroth lag is at that element.
	 (zeroth-lag (.length metric-accent-gaussian))
	 (positive-lag-range (list zeroth-lag (.length metric-ODF-correlation)))
	 ;; Find the peaks in the positive lag region of the cross-correlation.  
	 ;; Why do we only use positive lag regions? It took me a while to figure out and
	 ;; recall what I did also...  Positive lag values effectively indicate how far to
	 ;; shift the ODF candidate *backwards* in time, thereby determining the play region
	 ;; start time. Negative lag values indicate how far to shift the candidate *forward*
	 ;; in time, but since we are nowdays only using phrases within a theme, this means
	 ;; playing before the start of the candidate media, meaning leading black or silence
	 ;; (in AVM-Odf-Matcher's case) which would produce poor results (even if the
	 ;; correlation score was high).
	 (metric-ODF-correlation-peaks (extrema-points (.subseq metric-ODF-correlation positive-lag-range))))

    ;; Find that number of highest correlation measures, returning the locations in a
    ;; vector, in descending correlation order.
    (.subseq metric-ODF-correlation-peaks 0 acceptable-correlation-measures)))

(defun match-expectations-to-candidates (expectations candidate-rhythms)
  "Returns the measure of autocorrelation matches of the expectations against the
  candidate rhythms"
  (let* ((temporal-discrimination (* 0.060 sample-rate)) ; 60mSec best temporal discrimination.
	 (match-prototype (temporal-likelihood-of-expectations expectations temporal-discrimination)))
    ;; TODO since the candidate rhythms will not be expected to change often, we can
    ;; precalculate the Gaussian envelopes for each.
    (dolist (rhythm-envelope (gaussian-rhythm-envelopes candidate-rhythms))
      (cross-correlation-match rhythm-envelope match-prototype))))
