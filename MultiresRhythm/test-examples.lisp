;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing using various signals.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; See 
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

;;; (in-package multires-rhythm)

(defun fm-test (&key (signal-length 2048))
  "Frequency modulating signal for wavelet analysis."
  (let* ((carrierFreq 16)
	 (modFreq 2.1)
	 (norm-signal (.rseq 0 1 signal-length))
	 (mod (.* 3.0 (.cos (.* norm-signal 2.0 pi modFreq))))
	 (fm-signal (.cos (.+ (.* 2 pi carrierFreq norm-signal) mod))))
    ;; (plot mod nil)
    (plot fm-signal nil)
    fm-signal))

;; Test dyadic signals:
;; (multiple-value-setq (fm-mag fm-phase) (dyadic-cwt (fm-test) 8 512))
;; Test any length signals:
;; (multiple-value-setq (fm-mag fm-phase) (cwt (fm-test) 8))
;; Test plotting of magnitude only:
;; (plot-cwt (cwt (fm-test) 8))
;; (setf x (dyadic-cwt (fm-test :signal-length 256) 8 256))
;; Test plotting of both magnitude and phase:
;; (multiple-value-bind (fm-mag fm-phase) (cwt (fm-test) 16) (plot-cwt fm-mag fm-phase :title "fm"))

;;; 
(defun rising-harmonic-test (&key (signal-length 2048))
  "Analyse a harmonic (ie. cosinusoidal with harmonics) signal undergoing frequency change"
  ;; a nice set of JI ratios 1/1 + octave+8/5 + 2 octaves+6/5
  (let* ((norm-signal (.rseq 0 1 signal-length))
	 (periodicity (.+ (.* norm-signal 16) 4))
	 (periodicity2 (.* periodicity (+ 1 8/5)))
	 (periodicity3 (.* periodicity (+ 4 6/5)))
	 (rising-harmonic-signal (.+ (.* 0.33 (.cos (.* norm-signal 2.0 pi periodicity)))
				     (.* 0.33 (.cos (.* norm-signal 2.0 pi periodicity2)))
				     (.* 0.33 (.cos (.* norm-signal 2.0 pi periodicity3))))))
    (plot rising-harmonic-signal nil)
    rising-harmonic-signal))

;; (multiple-value-bind (rising-harmonic-mag rising-harmonic-phase) (cwt (rising-harmonic-test) 8) (plot-cwt rising-harmonic-mag rising-harmonic-phase :title "rising-harmonic"))

(defun test-lpc ()
  (multiple-value-bind (fm-mag fm-phase) (cwt (fm-test) 8)
    ;; (plot-cwt fm-mag fm-phase :title "fm")
    (plot-image #'magnitude-image "-lpc" (list (local-phase-congruency fm-mag fm-phase)) :title "fm")))
  
;; (test-lpc)

(defun test-rhythm ()
  (let* ((rhythm-signal (rhythmic-grid-to-signal '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0 1))))
    (multiple-value-bind (rhythm-mag rhythm-phase) (cwt rhythm-signal 8)
      (plot-cwt rhythm-mag rhythm-phase :title "rhythm"))))

;; (setf b (rhythmic-grid-to-signal '(1 1 1 1) :tempo 60))

(defun test-tactus ()
  (let ((shmulevich-rhythm 
	 (make-instance 'rhythm 
			:name "shmulevich-rhythm-1"
			:description "shmulevich rhythm 1" 
			:time-signal (rhythmic-grid-to-signal '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0 1) 
							      :sample-rate 200)
			:sample-rate 200)))
    (tactus-for-rhythm shmulevich-rhythm)))

;; (setf isochronous-rhythm (make-double-array (2048)))

