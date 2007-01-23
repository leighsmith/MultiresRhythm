;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing using various signals.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
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
;; (setf fm-scaleogram (cwt (fm-test) 16))
;; Test plotting of both magnitude and phase:
;; (plot-cwt (cwt (fm-test) 16) :title "fm")
;; (plot-cwt-labelled (cwt (fm-test) 16) :title "fm")

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
;; (plot-cwt-labelled (cwt (rising-harmonic-test) 8) :title "rising-harmonic")

(defun test-lpc ()
"Tests local phase congruency"
  (let ((fm-scaleogram (cwt (fm-test) 8)))
    ;; (plot-cwt fm-scaleogram :title "fm")
    (plot-image #'magnitude-image "-lpc" 
		(list (local-phase-congruency (scaleogram-magnitude fm-scaleogram)
					      (scaleogram-phase fm-scaleogram))) 
		:title "fm")))
  
;; (test-lpc)

(defun scaleogram-of-grid (rhythm-grid)
  (let* ((rhythm-scaleogram (scaleogram-of-rhythm 
			     (rhythm-of-grid "test-rhythm" rhythm-grid :shortest-ioi 256)
			     :voices-per-octave 12)))
    (plot-cwt-labelled rhythm-scaleogram :title "test-rhythm")
    rhythm-scaleogram))

;; (setf rhythm-scaleogram (scaleogram-of-grid '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0)))
;; (scaleogram-of-grid '(1 1 1 1 1 1 1 1 1 1))
;; (scaleogram-of-grid '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
;; (plot-scale-energy-at-time (scaleogram-of-grid '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 650)
;; (setf b (rhythm-of-grid '(1 1 1 1) :tempo 60))

(defun tactus-for-rhythm-grid (name rhythm-grid)
  (clap-to-rhythm (rhythm-of-grid name rhythm-grid :sample-rate 200 :tempo 80)))

;; (tactus-for-rhythm-grid "shmulevich-rhythm-1" '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0))
;;; Do the analysis on an impulse train.
;; (tactus-for-rhythm-grid "isochronous-rhythm"  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
;;; A little test that the dyadic number of impulses has no influence.
;; (tactus-for-rhythm-grid "isochronous-rhythm-longer"  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

(defun test-cwt-reconstruction (test-signal &key (voices-per-octave 8))
  "Verifies the invertability of the CWT and therefore it's accuracy."
  (let* ((test-scaleogram (cwt test-signal voices-per-octave)) ; (/ (.array-dimension test-signal 0) 4)
	 (reconstructed-signal (icwt (scaleogram-magnitude test-scaleogram)
				     (scaleogram-phase test-scaleogram) voices-per-octave)))
    ;;
    ;; The real component of the reconstructed signal is the original
    ;; signal, not the magnitude, since it's a Hilbert transform.
    ;; The phase is derived from the the real and imaginary components.
    ;; reconstructed-phase (.phase reconstructed-signal)
    ;;
    (nplot (list test-signal (.realpart reconstructed-signal)) nil 
	   :legends (list "test-signal" "reconstructed signal")
	   :title "Reconstruction comparison")
    (format t "maximum difference between test-signal and reconstruction ~f~%"
	    (.max (.- test-signal (.realpart reconstructed-signal))))
    reconstructed-signal))

;; Must be dyadic (base 2). Otherwise we must pad the signals.
;; (test-cwt-reconstruction (.cos (.rseq 0.0 (* 2 pi 16) 2048)))
;; (test-cwt-reconstruction (rising-harmonic-test))
;; Test non-dyadic reconstruction:
;; (test-cwt-reconstruction (fm-test :signal-length 2200))
;; Test reconstruction of a dirac function defined rhythmic signal.
;; (test-cwt-reconstruction (time-signal (rhythm-of-grid "test" '(1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0))))

(defun load-rhythm-file (filename &key (sample-rate 200) (description ""))
  (let* ((file-path (make-pathname 
		     :directory (list :absolute "/Users/leigh/Research/Data/NewAnalysedRhythms/") 
		     :name filename
		     :type "octave"))
	 (rhythm-signal (.load-octave-file file-path))
	 ;; Signals can be read in as column or row vectors, so we align them all to a row
	 ;; vector by transposing.
	 (aligned-signal (if (> (.array-dimension rhythm-signal 0) 1)
			     (.transpose rhythm-signal)
			     rhythm-signal)))
    (make-instance 'rhythm 
		   :name filename
		   :description description
		   :time-signal (.row aligned-signal 0)
		   :sample-rate sample-rate)))

(defun clap-to-octave-file (filename 
			 &key
			 (sample-rate 200) 
			 (description "") 
			 (tactus-selector #'select-longest-lowest-tactus))
  (let* ((loaded-rhythm (load-rhythm-file filename :description description :sample-rate sample-rate)))
    (clap-to-rhythm loaded-rhythm :tactus-selector tactus-selector)))

;; For decimating greensleeves to 200Hz.
;;(setf x (load-rhythm-file "greensleeves-perform-medium" :sample-rate 400))
;;(floor (.length (time-signal x)) 2)
;;(.floor (.find (time-signal x)) 2.0)

;; (clap-to-octave-file "longuet_cliche" :sample-rate 200)
;; (clap-to-octave-file "intensity34_to_44" :sample-rate 200)
;; (clap-to-octave-file "greensleeves-perform-medium" :sample-rate 400 :description "Greensleeves performed")

;; (clap-to-octave-file "desain-unquantized" :sample-rate 200)
;; (clap-to-octave-file "desain-unquantized" :sample-rate 200 :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time 58 62 skeleton)))
;; (clap-to-octave-file "desain-unquantized" :sample-rate 200 :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time 73 65 skeleton)))
;; (clap-to-octave-file "desain-quantized" :sample-rate 200)
;; (clap-to-octave-file "lengthen_corr" :sample-rate 200)

;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples 1500 :report :flat) (clap-to-octave-file "longuet_cliche" :sample-rate 200))

;;(setf loaded-rhythm (load-rhythm-file "longuet_cliche" :description "longuet" :sample-rate 200))
;;(setf (time-signal loaded-rhythm) (dyadic-pad (time-signal loaded-rhythm) :silence-pad t :padded-length 2048))
;;(clap-to-rhythm loaded-rhythm :tactus-selector #'select-longest-lowest-tactus)


;; (setf desain-at-65 (mapcar (lambda (x) (scale-at-time x 65)) desain-skeleton))
;; (loop for y in desain-at-65 when y collect y)


;; (setf cliche-rhythm (load-rhythm-file "longuet_cliche"))
;; (multiple-value-setq (cliche-tactus cliche-scaleogram) (tactus-for-rhythm cliche-rhythm))
;; (multiple-value-setq (cliche-skeleton cliche-scaleogram) (skeleton-of-rhythm cliche-rhythm))
;; (plot-cwt-labelled cliche-scaleogram)

;;; Needs to have remaining time 
(defun clap-to-iois (name iois &key (shortest-ioi (/ 120 17)))
  (clap-to-rhythm (iois-to-rhythm name iois :shortest-ioi shortest-ioi)))

; (onsets-to-grid (iois-to-onsets (intervals-in-samples '(17 17 20.5 13.5) :ioi (/ 600 17))))
; (onsets-to-grid (iois-to-onsets (intervals-in-samples '(17 17 20.5 13.5 17 17) :ioi 10)))
; (iois-to-rhythm "test" '(17 17 20.5 13.5 17 17) :shortest-ioi (/ 120 17))

;; Gouyon & Dixon's examples of ambiguity of timing/tempo changes.
;; (clap-to-iois "isochronous" '(17 17 17 17 17 17))
;; (clap-to-iois "local-timing" '(17 17 20.5 13.5 17 17))
;; (clap-to-iois "global-timing" '(17 17 20.5 17 17 17))
;; (clap-to-iois "tempo-change" '(17 17 20.5 20.5 20.5 20.5))

;;;  3 against 2 polyrhythm example from Sethares & Staley 2001
;;; (clap-to-rhythm (add-rhythm (iois-to-rhythm "duple" (repeat-rhythm '(100 100 100 100) 4) :sample-rate 200)
;;;	                        (iois-to-rhythm "triple" (repeat-rhythm '(67 67 66 67 67 66) 4) :sample-rate 200)))
;;;
;;; (multiple-value-bind (tactus rhythm-scaleogram) (tactus-for-rhythm polyrhythm)
;;;		   (plot-scale-energy-at-time rhythm-scaleogram 236))
;;;
;;; TODO need to increase tempo by 5% every eight beats. Should result in an increase in
;;; tempo of more than 25% over 15 seconds.


;;; From Desain & Honing 1999
;;; (clap-to-iois "desain99" '(3 1 6 2 3 1 6 2 3 1) :shortest-ioi 50)
;;; (clap-to-rhythm (iois-to-rhythm "desain99" '(3 1 6 2 3 1 6 2 3 1) :shortest-ioi 50)
;;;                 :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time 91 0 skeleton)))
;;;
;;; TODO need to generate claps as scorefile from clap-to-rhythm

;;; National Anthem data-base
(load "/Volumes/iDisk/Research/Data/DesainHoning/national anthems.lisp")

(defun anthem-rhythm (anthem-number &key (shortest-ioi 50))
  (let ((anthem-name (symbol-name (first (first (nth anthem-number *national-anthems*)))))
	(anthem-iois (second (nth anthem-number *national-anthems*))))
    (iois-to-rhythm anthem-name anthem-iois :shortest-ioi shortest-ioi)))

(defun clap-to-anthem (anthem-number)
    (clap-to-rhythm (anthem-rhythm anthem-number :shortest-ioi 50)))

;; Seconds
;(0.305d0 0.375d0 0.375d0 0.445d0 )
;(0.305d0 0.375d0 0.445d0)
;; Ratios
;(0.813 1.0d0 1.0d0 1.186)
;(0.813 1.0d0 1.186)

;; Agogic phrasing.
;; (clap-to-rhythm (iois-to-rhythm "4-beat-lengthen-corr" (repeat-rhythm '(61 75 75 89) 12)))
;; (clap-to-rhythm (iois-to-rhythm "3-beat-lengthen-corr" (repeat-rhythm '(61 75 89) 16)))
;; (clap-to-octave-file "lengthen_corr" :sample-rate 200)

;;; For testing plotting
;; (setf plot-test-rhythm (load-rhythm-file "intensity34_to_44" :description "intensity" :sample-rate 200))
;; (multiple-value-setq (plot-test-tactus plot-test-scaleogram) (tactus-for-rhythm plot-test-rhythm))
;; (plot-cwt+tactus-labelled plot-test-scaleogram plot-test-tactus plot-test-rhythm :phase-palette :grey-smooth)

