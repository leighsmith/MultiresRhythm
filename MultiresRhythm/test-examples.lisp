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

(in-package :multires-rhythm)
(use-package :nlisp)

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
;; (setf x (dyadic-cwt (fm-test :signal-length 256) 8 256)) ; magnitude only.
;; Test any length signals:
;; (setf fm-scaleogram (cwt (fm-test) 8))
;; Test plotting of both magnitude and phase:
;; (plot-cwt (cwt (fm-test) 16) :title "fm")

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

;; (plot-cwt (cwt (rising-harmonic-test) 8) :title "rising-harmonic")

(defun test-lpc ()
  (let ((fm-scaleogram (cwt (fm-test) 8)))
    ;; (plot-cwt fm-scaleogram :title "fm")
    (plot-image #'magnitude-image "-lpc" 
		(list (local-phase-congruency (scaleogram-magnitude fm-scaleogram)
					      (scaleogram-phase fm-scaleogram))) 
		:title "fm")))
  
;; (test-lpc)

(defun test-rhythm ()
  (let* ((rhythm-signal (rhythmic-grid-to-signal '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0 1)))
	 (rhythm-scaleogram (cwt rhythm-signal 8)))
    (plot-cwt rhythm-scaleogram :title "rhythm")))

;; (test-rhythm)
;; (setf b (rhythmic-grid-to-signal '(1 1 1 1) :tempo 60))

(defun test-tactus-for-rhythm (name rhythm-grid)
  (let ((test-rhythm 
	 (make-instance 'rhythm 
			:name name
			:description name ; TODO transliterate '-' for ' '.
			:time-signal (rhythmic-grid-to-signal rhythm-grid :sample-rate 200)
			:sample-rate 200)))
    (tactus-for-rhythm test-rhythm)))

;; (test-tactus-for-rhythm "shmulevich-rhythm-1" '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0 1))
;;; Do the analysis on an impulse train.
;; (test-tactus-for-rhythm "isochronous-rhythm"  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

;;;
(defun test-reconstruction (test-signal &key (voices-per-octave 8))
  "Verifies the invertability of the CWT and therefore it's accuracy."
  (let* ((test-scaleogram (cwt test-signal voices-per-octave)) ; (/ (.array-dimension test-signal 0) 4)
	 (reconstructed-signal (icwt (scaleogram-magnitude test-scaleogram)
				     (scaleogram-phase test-scaleogram) voices-per-octave)))

    ;; The real component of the reconstructed signal is the original
    ;; signal, not the magnitude, since it's a Hilbert transform.
    ;; The phase is derived from the the real and imaginary components.
    ;; reconstructed-phase (.phase reconstructed-signal)
  
    (nplot (list test-signal (.realpart reconstructed-signal)) nil 
	   :legends (list "test-signal" "reconstructed signal")
	   :title "Reconstruction comparison")

    (format t "maximum difference between test-signal and reconstruction ~f~%"
	    (.max (.- test-signal (.realpart reconstructed-signal))))
    reconstructed-signal))

;; Must be dyadic (base 2). Otherwise we must pad the signals.
;; (test-reconstruction (.cos (.rseq 0.0 (* 2 pi 16) 2048)))
;; (test-reconstruction (rising-harmonic-test))
;; Test non-dyadic reconstruction:
;; (test-reconstruction (fm-test :signal-length 2200))

(defun test-octave-file (filename &key (sample-rate 200 sample-rate-supplied))
  (let* ((file-path (make-pathname :directory (list :absolute "/Users/leigh/Research/Data/NewAnalysedRhythms/") 
				   :name filename
				   :type "octave"))
	 (rhythm-signal (.load-octave-file file-path))
	 (loaded-rhythm (make-instance 'rhythm 
				       :name filename
				       :description "Greensleeves performed" 
				       :time-signal (.row rhythm-signal 0)
				       :sample-rate sample-rate))
	 (computed-tactus (tactus-for-rhythm loaded-rhythm)))
    computed-tactus))

;; (test-octave-file "greensleeves-perform-medium" :sample-rate 400)
;; (test-octave-file "longuet_cliche" :sample-rate 200)
;; (test-octave-file "intensity34_to_44" :sample-rate 200)