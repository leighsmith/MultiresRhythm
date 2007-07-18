;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing using National Anthem data-base.
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

;; To get anthems with an anacrusis:
;; (remove-if #'zerop *national-anthems* :key #'anthem-start-at)
;; How many rhythms have an anacrusis?
;; (/ (length (remove-if #'zerop *national-anthems* :key #'anthem-start-at)) (length *national-anthems*)))

(defun plot-anacrusis-profiles (rhythm first-notes)
  "Plots the profiles of the first first-notes number of notes in the rhythm"
  ;; retrieve the first set of onset times of the anacrusis.
  (let ((times-in-samples (nlisp::array-to-list (.subarray (onsets-in-samples rhythm)
							   (list 0 (list 0 (1- first-notes))))))
	(scaleogram (scaleogram-of-rhythm rhythm)))
    (format t "times-in-samples ~a~%" times-in-samples)
    (plot-scale-energy-at-times scaleogram times-in-samples :description (name rhythm))))

;; (plot-anacrusis-profiles (anthem-rhythm (anthem-named 'netherlands)) (downbeat-number (anthem-named 'netherlands))

(defun ridge-ratios-at-time (ridges datum-ridge note-time &key (voices-per-octave 16))
  "Return the ratios of ridges to some datum ridge at each given time point"
  (let* ((datum-time-support (time-support (scale-at-time datum-ridge note-time) voices-per-octave))
	 (ratios-to-datum (mapcar (lambda (ridge-of-note) 
				    (/ (time-support (scale-at-time ridge-of-note note-time) voices-per-octave) datum-time-support))
				  ridges)))
    (format t "datum-time-support ~f~%" datum-time-support)
    (sort ratios-to-datum #'<)))

(defun anthem-anacrusis (anthem &key (length-limit 16384))
  "Plots the profiles of the first first-notes number of notes in the rhythm"
  ;; retrieve the first set of onset times of the anacrusis.
  (multiple-value-bind (skeleton scaleogram ridge-scale-peaks)
      (skeleton-of-rhythm (limit-rhythm (anthem-rhythm anthem) :maximum-samples length-limit))
    (let* ((description (anthem-name anthem))
	   (anacrusis-note-times (anacrusis-notes anthem))
	   (times-in-samples (nlisp::array-to-list anacrusis-note-times))
	   (local-pc (local-phase-congruency (scaleogram-magnitude scaleogram) (scaleogram-phase scaleogram)))
	   (lpc-at-time (./ (.partial-sum local-pc) (.array-dimension local-pc 0)))
	   (pc (phase-congruency (scaleogram-magnitude scaleogram) (scaleogram-phase scaleogram)))
	   (ridges-on-notes (mapcar (lambda (time) (ridges-at-time skeleton time)) times-in-samples)))
      (format t "times-in-samples ~a~%" times-in-samples)
      (format t "number of ridges at notes ~a~%" (mapcar #'length ridges-on-notes))
      (format t "phase congruency at notes ~a~%" (.arefs pc anacrusis-note-times))
      (format t "summed local phase congruency at notes ~a~%" (.arefs lpc-at-time anacrusis-note-times))
      (format t "ridge ratios ~a~%" (ridge-ratios-at-time (first ridges-on-notes) (caar ridges-on-notes) (first times-in-samples)))
      (plot-highlighted-ridges scaleogram '() ridge-scale-peaks :title description)
      (window)
      (nplot (list pc) nil :title (format nil "phase congruency of ~a" description) :aspect-ratio 0.66)
      (window)
      (let ((bar-scale-index (- (number-of-scales scaleogram)
				(round (bar-scale anthem (voices-per-octave scaleogram)))))
	    (beat-scale-index (- (number-of-scales scaleogram)
				 (round (beat-scale anthem (voices-per-octave
							    scaleogram))))))
	(format t "beat scale index ~a, bar scale index ~a~%" beat-scale-index bar-scale-index)
	(reset-plot)
	(plot-command "set arrow 1 from ~d,0.2 to ~d,0.15" bar-scale-index bar-scale-index)
	(plot-command "set arrow 2 from ~d,0.2 to ~d,0.15" beat-scale-index beat-scale-index)
	(plot-command "show arrow 1")
	(plot-command "show arrow 2")
	(plot-scale-energy-at-times scaleogram times-in-samples :description description))
      ridges-on-notes)))

;; (setf american-ridges (anthem-anacrusis (anthem-named 'america)))
;; (setf australian-ridges (anthem-anacrusis (anthem-named 'australia)))
;; (anthem-anacrusis (anthem-named 'vietnam))
;; (anthem-anacrusis (anthem-named 'finland))

(defgeneric accent-rhythm (rhythm-to-accent start-onset period &key accent))

(defmethod accent-rhythm ((rhythm-to-accent rhythm) start-onset period &key (accent 1.25d0))
  "Scales certain beats in a rhythm, accenting every period-th onset, starting from
start-onset, these measures are in samples"
  ;; scale down all the other beats
  (let* ((rhythm-sample-length (duration-in-samples rhythm-to-accent))
	 (duration-of-metric-region (- rhythm-sample-length start-onset))
	 (time-signal (time-signal rhythm-to-accent))
	 (onset-times (.floor (.rseq start-onset 
				     (- rhythm-sample-length period)
				     (/ duration-of-metric-region period))))
	 (scaled-notes (.* (.arefs time-signal onset-times) accent))
	 (scaled-time-signal (.* time-signal (/ 1 accent))))
    (setf (.arefs scaled-time-signal onset-times) scaled-notes)
    (setf (time-signal rhythm-to-accent) scaled-time-signal)
    rhythm-to-accent))

(defun accented-anthem-rhythm (anthem)
  "Accents the anthem rhythm according to the transcribed anacrusis and measure"
  (accent-rhythm (anthem-rhythm anthem) 
		 (anthem-duration-in-samples anthem (anthem-anacrusis-duration anthem))
		 (anthem-duration-in-samples anthem (anthem-bar-duration anthem))))

(defun plot-scaleogram-skeleton-of-anthem-accented (anthem &key (voices-per-octave 16))
  "Plot the ridges in greyscale and the highlighted ridges in red."
  (let ((anthem-rhythm (accented-anthem-rhythm anthem)))
    (multiple-value-bind (skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave)
      (let* ((time-axis-decimation 4)
	     (formatting "set title font \"Times,20\"~%set xlabel font \"Times,20\"~%set ylabel font \"Times,20\"")
	     (axes-labels (axes-labelled-in-seconds rhythm-scaleogram (sample-rate anthem-rhythm) time-axis-decimation))
	     (highlighted-ridges (list (canonical-bar-ridge anthem rhythm-scaleogram))))
      (format t "Plotting images~%")
      (plot-images (list (list #'magnitude-image 
			       (list (scaleogram-magnitude rhythm-scaleogram))
			       '((1.0 1.0) (0.0 0.3))
			       (concatenate 'string axes-labels formatting))
			 (list #'highlighted-ridges-image
			       (list (mapcar #'copy-object highlighted-ridges)  correlated-ridge-scale-peaks)
			       '((1.0 1.0) (0.0 0.0))
			       (concatenate 'string axes-labels formatting))) ; ':xlabel "Time (Seconds)"
		   :title (name anthem-rhythm)
		   :time-axis-decimation time-axis-decimation)))))

;; (plot-scaleogram-skeleton-of-anthem-accented (anthem-named 'america))

(defun long-silence-dyadic-pad (signal &key (silence-pad t))
  "Pad at double the dyadic length"
  (pad-signal signal (dyadic-length (1+ (dyadic-length (.length signal)))) :silence-pad silence-pad))

;; Retrieve the first region of rhythm (or do we have to do the whole thing to get
;; sufficient lower frequency regions?) this would seem problematic for inducing an
;; anacrusis. We should be able to set a maximum time region covering beat or bar period.
(defmethod scaleogram-of-rhythm-silence ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  (format t "Length of Rhythm ~f seconds~%" (duration analysis-rhythm))
  (let ((silence-padded-rhythm (long-silence-dyadic-pad (time-signal analysis-rhythm) :silence-pad t)))
    (cwt silence-padded-rhythm voices-per-octave)))

;; (setf scaleogram (scaleogram-of-rhythm-silence (anthem-rhythm (anthem-named 'america))))
;; (setf scaleogram (scaleogram-of-rhythm-silence (anthem-rhythm (anthem-named 'australia))))
;; (plot-cwt scaleogram)

;; (setf correlated-ridge-scale-peaks (scale-peaks-of-scaleogram scaleogram (sample-rate analysis-rhythm)))
;;        	 (skeleton (make-instance 'skeleton 
;; 				  :ridges (extract-ridges correlated-ridge-scale-peaks)
;; 				  :duration (duration-in-samples scaleogram)
;; 				  :scales (number-of-scales scaleogram)
;; 				  :voices-per-octave (voices-per-octave scaleogram)
;; 				  :skip-highest-octaves (skip-highest-octaves scaleogram))))
;;     (values skeleton scaleogram correlated-ridge-scale-peaks)))
