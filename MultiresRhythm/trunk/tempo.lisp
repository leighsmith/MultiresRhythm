;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;; 
;;;; Routines to determine tempo preference.
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

;; Fastest scale for tactus 
;; (scale-from-period (* 200 0.25) 16)
;; Slowest scale for tactus
;; (scale-from-period (* 200 2.0) 16)
;; Slowest scale "accessible to the senses" (London 2004)
;; (scale-from-period (* 200 5.0) 16)

(defun preferred-tempo-scale (voices-per-octave rhythm-sample-rate 
			      ;; Fraisse's "spontaneous tempo" interval in seconds
			      &key (tempo-salience-peak 0.600))
  "Determine the scale which Fraisse's spontaneous tempo would occur at.
This is weighted by absolute constraints, look in the 600ms period range."
  (let* ((salient-IOI (* rhythm-sample-rate tempo-salience-peak)))
    ;; Convert salient-IOI into the scale to begin checking.
    ;; This assumes the highest frequency scale (shortest time support)
    ;; is index 0, lowest frequency (longest time support) is number-of-scales.
    (floor (scale-from-period salient-IOI voices-per-octave))))

;;; Scale index 0 is the highest frequency (smallest dilation) scale.
(defun tempo-salience-weighting-vector (salient-scale number-of-scales &key (voices-per-octave 16)
					;; Define a doubling in frequency, 1 octave as 1 stddev.
					(octaves-per-stddev 1.0)
					(envelope #'gaussian-envelope))
  "Returns a tempo weighting vector"
  ;; (format t "tempo-salience-weighting-vector salient scale is ~a~%" salient-scale)
  (let* ((stddev-span 10.0)) ; Match the mean to a span across -5 < mean < 5 standard deviations.
    ;; Create a Gaussian envelope spanning the number of scales.
    (funcall envelope number-of-scales 
	     :mean (- (/ (* stddev-span salient-scale) number-of-scales) 
		      (/ stddev-span 2.0))
	     :stddev (/ (* voices-per-octave stddev-span octaves-per-stddev) number-of-scales)
	     :scaling 1d0)))

;;; Scale index 0 is the highest frequency (smallest dilation) scale.
(defun tempo-salience-weighting (salient-scale time-frequency-dimensions &rest tempo-arguments)
  "Produce a weighting matching the analysis window using tempo preference."
  (let* ((number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (tempo-weighting-over-time (make-double-array time-frequency-dimensions))
	 ;; Create a Gaussian envelope spanning the number of scales.
	 (tempo-scale-weighting (apply #'tempo-salience-weighting-vector salient-scale number-of-scales tempo-arguments)))
    (dotimes (time time-in-samples)
      (setf (.subarray tempo-weighting-over-time (list t time))
	    (.reshape tempo-scale-weighting (list number-of-scales 1))))
    tempo-weighting-over-time))

;; (plot (tempo-salience-weighting-vector 78 144 :voices-per-octave 16.0) nil :title "Preferred tempo weighting profile" :aspect-ratio 0.66)

;;; Scale index 0 is the highest frequency (smallest dilation) scale.
;;; this is weighted by the log scale character, rather than symmetrical.
(defun tempo-salience-weighting-log (salient-scale time-frequency-dimensions &key (voices-per-octave 16))
  "Produce a weighting matching the analysis window using tempo preference. This weighting
   is skewed so that the scales higher than the mean are half as likely as scales lower than the mean."
  (tempo-salience-weighting salient-scale time-frequency-dimensions 
			    :voices-per-octave voices-per-octave
			    :envelope #'skewed-gaussian-envelope))

;;;;
;;;; Implementation of van Noorden & Moelants resonance model
;;;;

;;; Computes the effective resonance amplitude (van Noorden & Moelants p46 equation 3) as
;;; a function of rhythmic frequency.
(defun tempo-resonance-curve (resonant-frequency damping-factor 
			      ;; independent variable, in seconds
			      &key (external-period (.rseq 0.2d0 1.5d0 (floor (- 1.5d0 0.2d0) 0.020d0))))
  "Returns the tempo resonance curve given the parameters, sampled at a given sample rate"
  (let* ((external-frequency (./ 1.0d0 external-period))
	 (squared-resonant-frequency (* resonant-frequency resonant-frequency))
	 (squared-external-frequency (.* external-frequency external-frequency)))
    (.- (./ 1.0d0 (.sqrt (.+ (.expt (.- squared-resonant-frequency squared-external-frequency) 2)
			     (.* damping-factor squared-external-frequency))))
	(./ 1.0d0 (.sqrt (.+ (* squared-resonant-frequency squared-resonant-frequency)
			     (.* squared-external-frequency squared-external-frequency)))))))

;; Test with van Noorden & Moelants' parameters.
;; (let ((external-period (.rseq 0.2d0 1.5d0 (floor (- 1.5d0 0.2d0) 0.020d0))))
;;   (plot (tempo-resonance-curve (/ 1 0.5d0) 1.12 :external-period external-period) external-period))

;; With periods derived from time support of scaled wavelet kernels. 
;; (let ((external-period (time-support-seconds (.iseq 0 128) 16 200.0)))
;;  (plot (tempo-resonance-curve (/ 1 0.5d0) 1.12 :external-period external-period) external-period))
