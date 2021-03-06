;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Plotting methods for scaleograms.
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

(defgeneric plot-cwt (scaleogram &key title time-axis-decimation)
  (:documentation "Function to plot the magnitude and phase components of the result of a continuous wavelet transform on a signal using gnuplot with labelling."))

(defgeneric plot-cwt-of-rhythm (scaleogram rhythm &key title time-axis-decimation)
  (:documentation "Function to plot the magnitude and phase components of the result of a continuous wavelet transform on a rhythm signal."))

(defgeneric plot-cwt+ridges (scaleogram ridges rhythm &key title time-axis-decimation)
  (:documentation "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase overlaid with the tactus in white."))

(defgeneric plot-cwt+ridges-of (scaleogram-to-plot highlighted-ridges analysis-rhythm 
						   &key title time-axis-decimation)
  (:documentation "Plot highlighted ridges over the magnitude and ridges plots of the given rhythm."))

(defgeneric plot-cwt+skeleton-of (analysis highlighted-ridges analysis-rhythm 
					   &key title time-axis-decimation)
  (:documentation "Plot all ridges in greyscale and the highlighted ridges in red."))

(defgeneric plot-scale-energy-at-times (scaleogram times &key description)
  (:documentation "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."))


;; (defgeneric label-scale-as-time-support (scaleogram)
;;   (:documentation "Returns a list of plotting labels giving the time support for each scale"))

;; (defgeneric label-scale-as-time-support-seconds (scaleogram sample-rate)
;;   (:documentation "Generates a set of labels of the scales as time support intervals in seconds"))

;;;; Methods

;; We reverse the labels so we plot in more intuitive lowest scale on the left orientation.
(defun label-scale-as-time-support (scaleogram-to-plot)
  "Generates a set of labels of the scales as time support interval"
  (let* ((vpo (voices-per-octave scaleogram-to-plot))
	 (scale-number-per-octave (.* (.iseq 0 (number-of-octaves scaleogram-to-plot)) vpo))
	 (time-support-per-octave (.floor (time-support scale-number-per-octave vpo))))
    (loop 
       for label across (val (.reverse time-support-per-octave))
       for position across (val scale-number-per-octave)
       collect (list label position)))) ; Should return label as a string.

;; Works with scaleogram or skeleton.
(defun label-scale-as-time-support-seconds (scaleogram-to-plot sample-rate)
  "Generates a set of labels of the scales as time support intervals in seconds"
  (mapcar (lambda (label-and-index) 
	    (cons (/ (first label-and-index) sample-rate) (rest label-and-index)))
	  (label-scale-as-time-support scaleogram-to-plot)))

;; We use the characters from the Sonata Postscript font
(let ((beats '(;;("r"   0.125)		; r = 32nd somehow doesn't work properly.
	       ("x3"  0.1667)
	       ;; ("r ." 0.1875) 
	       ("x"   0.25)		; x = sixteenth
	       ("x ." 0.375) 
	       ("e"   0.5)		; e = eighth note
	       ("e ." 0.75) 
	       ("q"   1)		; q = quarter note
	       ("q ." 1.5) 
	       ("h"   2)		; h = half note
	       ("h ." 3) 
	       ("w"   4)		; w = whole note
	       ("w ." 6) 
	       ("W"   8)		; W = breve (two whole notes)
	       ("WW"  16))))		; WW = tied breves

  (defun label-scale-as-rhythmic-beat (voices-per-octave crochet-IOI)
    "Label the scale axes in notated rhythmic units, from the current tempo."
    ;; determine the IOI for a crochet at the given tempo
    ;; (crochet-IOI (samples-per-crochet tempo)))
    (mapcar (lambda (x)     ; build the label, scaling by the crochet-IOI
	      ;; (list (format nil "{/Sonata ~a}" (first x))
	      (list (format nil "~a" (first x))
		    (scale-from-period (* (second x) crochet-IOI) voices-per-octave))) beats)))

;; (defmethod label-scale-as-rhythmic-beat ((scaleogram-to-plot scaleogram) crochet-IOI)
;; (label-scale-as-rhythmic-beat (voices-per-octave scaleogram-to-plot) crochet-IOI))

(defun axes-labelled-in-seconds (scaleogram-to-plot sample-rate time-axis-decimation)
  "Returns the axes commands to gnuplot to label axes in seconds"
  (let ((axes-commands (make-array 200 :element-type 'character :fill-pointer 0)))
    ;; ensures last label plotted
    ;; (format axes-commands "set xrange [0:~d]~%" (duration-in-samples scaleogram-to-plot))
    ;; Label plots in seconds.
    (format axes-commands "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
	    (label-samples-as-seconds (duration-in-samples scaleogram-to-plot)
				      sample-rate
				      :time-axis-decimation time-axis-decimation))
    (format axes-commands "set ytics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
	    (label-scale-as-time-support-seconds scaleogram-to-plot sample-rate))
    axes-commands))

(defun axes-labelled-in-samples (scaleogram-to-plot time-axis-decimation)
  "Returns the axes commands to gnuplot to label axes in samples"
  (let ((axes-commands (make-array 200 :element-type 'character :fill-pointer 0)))
    ;; ensures last label plotted
    ;; (format axes-commands "set xrange [0:~d]" (duration-in-samples scaleogram-to-plot))
    (format axes-commands "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" 
	    (label-samples (duration-in-samples scaleogram-to-plot) :time-axis-decimation time-axis-decimation))
    (format axes-commands "set ytics (~{~{\"~a\" ~5d~}~^, ~})~%" 
	    (label-scale-as-time-support scaleogram-to-plot))
    axes-commands))

(defmethod plot-cwt ((scaleogram-to-plot scaleogram) &key 
		     (title "unnamed") 
		     (time-axis-decimation 4))
  "Method to plot the magnitude and phase components of the result of
   a continuous wavelet transform on an arbitary signal."
  ;; Setting the X and Y tics and axes ranges.
  (let ((axes-labels (axes-labelled-in-samples scaleogram-to-plot time-axis-decimation)))
    (plot-images (list (list #'magnitude-image 
			     (list (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.5))
			     axes-labels)
		       (list #'phase-image 
			     (list (scaleogram-phase scaleogram-to-plot) (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.0))
			     axes-labels))
		 :title title
		 ;; :title (format nil "Scaleogram Magnitude of ~a" title)
		 :time-axis-decimation time-axis-decimation)))

(defmethod plot-cwt-of-rhythm ((scaleogram-to-plot scaleogram) (analysis-rhythm rhythm) &key 
			       (title (name analysis-rhythm))
			       (time-axis-decimation 4))
  "Method to plot the magnitude and phase components of the result of
   a continuous wavelet transform on a rhythm signal."
  ;; Setting the X and Y tics and axes ranges. Problem is this is set before (window) and
  ;; multiplot in plot-images. Either we must pass the function and parameters into
  ;; plot-images, or we create a delayed promise of invocation, in this case by passing in
  ;; the commands for later transfer to gnuplot.
  (let ((axes-labels (axes-labelled-in-seconds scaleogram-to-plot (sample-rate analysis-rhythm) time-axis-decimation)))
    ;; TODO ideally use (plot-rhythm analysis-rhythm)) for plotting the rhythm
    ;; Put the magnitude plot above the phase on the same window.
    (plot-images (list (list #'rhythm-plot
 			     (list analysis-rhythm)
			     '((0.95 0.5) (0.05 0.6))
			     (axes-labelled-in-seconds scaleogram-to-plot (sample-rate analysis-rhythm) 1))
		       (list #'magnitude-image 
			     (list (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.3))
			     axes-labels)
		       (list #'phase-image 
			     (list (scaleogram-phase scaleogram-to-plot) (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.0))
			     axes-labels))
		 :title title
		 :time-axis-decimation time-axis-decimation)))

;;; TODO this should be plot-cwt specialised on scaleogram 
(defmethod plot-cwt+ridges ((scaleogram-to-plot scaleogram)
			    (ridges list)
			    (analysis-rhythm rhythm)
			    &key 
			    (title (name analysis-rhythm))
			    (time-axis-decimation 4))
;;			    (magnitude-palette :greyscale)
;;			    (phase-palette :spectral)
  "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase
   overlaid with the tactus in white."
  (let ((axes-labels (axes-labelled-in-seconds scaleogram-to-plot (sample-rate analysis-rhythm) time-axis-decimation)))
    (plot-images (list  ;(list #'rhythm-plot
 			;     (list analysis-rhythm)
 			;    '((1.0 0.25) (0.0 0.3))
 			;    axes-labels)
		       (list #'magnitude-image 
			     (list (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.3))
			     axes-labels)
		       ;; :palette magnitude-palette
		       ;; We make a copy of the tactus since decimate modifies the object
		       ;; (it is, after all, an instance method) 
		       (list #'ridges-on-phase-image 
			     (list (mapcar #'copy-object ridges) 
				   (scaleogram-phase scaleogram-to-plot)
				   (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.0))
			     axes-labels))
		 :title title
		 :time-axis-decimation time-axis-decimation)))

;;; TODO this may not be necessary at all.
(defmethod plot-cwt+ridges-of ((scaleogram-to-plot scaleogram)
			       (highlighted-ridges list)
			       (analysis-rhythm rhythm)
			       &key 
			       (title "unnamed")
			       (time-axis-decimation 4))
  "Plot the magnitude in greyscale and the highlighted ridges in red."
  (let ((axes-labels (axes-labelled-in-seconds scaleogram-to-plot (sample-rate analysis-rhythm) time-axis-decimation)))
    (plot-images (list (list #'highlighted-ridges-image 
			     (list (mapcar #'copy-object highlighted-ridges)
				   (scaleogram-magnitude scaleogram-to-plot))
			     '((1.0 0.5) (0.0 0.3))
			     axes-labels))
		 :title title
		 :time-axis-decimation time-axis-decimation)))

;;; TODO this needs to be fully converted to use methods of image-plot class.
(defmethod plot-cwt+skeleton-of ((analysis multires-analysis)
				 (highlighted-ridges list)
				 (analysed-rhythm rhythm)
				 &key 
				 (title (name analysed-rhythm))
				 (time-axis-decimation 4))
  "Plot the ridges in greyscale and the highlighted ridges in red."
  (let* ((rhythm-scaleogram (scaleogram analysis))
	 (correlated-ridge-scale-peaks (ridge-peaks analysis))
	 (formatting "set title font \"Times,20\"~%set xlabel font \"Times,20\"~%set ylabel font \"Times,20\"")
	 (axes-labels (axes-labelled-in-seconds rhythm-scaleogram (sample-rate analysed-rhythm) time-axis-decimation))
	 (all-formatting (concatenate 'string axes-labels formatting)))
      (open-multiplot)
      (plot-magnitude (scaleogram-magnitude rhythm-scaleogram)
		      :window-dimensions '((1.0 0.5) (0.0 0.3))
		      :axes-labels all-formatting
		      :title (format nil "Scaleogram Magnitude of ~a" title)
		      :xlabel "")
      ;; If highlighted-ridges list is empty, just plot the ridge scale
      ;; peaks without the highlighted palette.
      (if highlighted-ridges
	  (plot-image #'highlighted-ridges-image
		(list (mapcar #'copy-object highlighted-ridges) correlated-ridge-scale-peaks)
		'((1.0 0.5) (0.0 0.0))
		all-formatting
		:title (format nil "Skeleton of ~a" title))
	  (plot-magnitude correlated-ridge-scale-peaks
		:window-dimensions '((1.0 0.5) (0.0 0.0))
		:axes-labels all-formatting
		:title (format nil "Skeleton of ~a" title)
		:xlabel "Time (Seconds)"))
      (close-multiplot)))

(defmethod plot-scale-energy-at-times ((scaleogram-to-plot scaleogram) times &key
				       (description "unnamed")
				       (sample-rate nil))
  "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."
  ;; (reset-plot)
  (plot-command "set title font \"Times,20\"")
  (plot-command "set xlabel font \"Times,20\"")
  (plot-command "set ylabel font \"Times,20\"")
  ;; (plot-command "set key off")
  (if (not sample-rate)
      (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support scaleogram-to-plot))
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-scale-as-time-support-seconds scaleogram-to-plot sample-rate)))
  ;; We reverse the column so we plot in more intuitive lowest scale on the left orientation.
  (plot (mapcar (lambda (time) (.reverse (.column (scaleogram-magnitude scaleogram-to-plot) time))) times)
	 nil 
	 :title (format nil "Energy profiles at select times of ~a" description)
	 :legends times ; "sample number ~d" 
	 :xlabel "Scale as IOI Range in Samples"
	 :ylabel "Magnitude Energy"
	 :reset nil
	 :aspect-ratio 0.2))

(defmethod plot-scale-energy+peaks-at-time ((scaleogram-to-plot scaleogram) time peaks &key (sample-rate nil))
  "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."
  (reset-plot)
  (plot-command "set title font \"Times,20\"")
  (plot-command "set xlabel font \"Times,20\"")
  (plot-command "set ylabel font \"Times,20\"")
  (plot-command "set key off")
  (if (not sample-rate)
      (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support scaleogram-to-plot))
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-scale-as-time-support-seconds scaleogram-to-plot sample-rate)))
  (let ((time-slice (.column (scaleogram-magnitude scaleogram-to-plot) time)))
    ;; We reverse the column so we plot in more intuitive lowest scale on the left orientation.
    (plot (list (.reverse time-slice) 
		 ;; scale the peaks down to magnitude maximum.
		 (.* (.max time-slice) (.reverse (.column peaks time)))) 
	   nil 
	   :title (format nil "Energy profile and scale peaks at sample number ~d" time)
	   :xlabel "Scale as IOI Range in Samples"
	   :ylabel "Magnitude Energy"
	   :styles '("lines" "impulses")
	   :reset nil
	   :aspect-ratio 0.2)))

(defun plot-voice-behaviour (original-rhythm scaleogram-to-plot voices)
  "Plots the magnitude response of several single wavelet voices (dilation scale) to a rhythm."
  (let ((scaleogram-mag (scaleogram-magnitude scaleogram-to-plot)))
    (plot (append (mapcar (lambda (voice) (.row scaleogram-mag voice)) voices)
		   (list (.* (.max scaleogram-mag) (onset-time-signal original-rhythm))))
	   nil
	   :legends (append (mapcar (lambda (voice) (format nil "Voice ~a" voice)) voices) 
			    (list (name original-rhythm)))
	   :title "Select wavelet voice behaviours to non-isochronous rhythms"
	   :aspect-ratio 0.15)))

(defun plot-tempo-preference (tempo-preference scaleogram sample-rate &key (weighting 1d0))
  "Plots the tempo weighting profile used"
  (reset-plot)
  (plot-command "set title font \"Times,24\"")
  (plot-command "set xlabel font \"Times,24\"")
  (plot-command "set ylabel font \"Times,24\"")
  (plot-command "set key off")
  (plot-command "set ytics 0.25 font \"Times,18\"")
  (plot-command "set xtics out font \"Times,18\" (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		(label-scale-as-time-support-seconds scaleogram sample-rate))
  ;; (format t "max plot ~a~%" (.max tempo-preference))
  (plot (.reverse (.* tempo-preference weighting))
	nil 
	:styles '("lines linetype 3 linewidth 2")
	:title "Tempo Preference Weighting Profile"
	:xlabel "Scale as IOI Range in Seconds"
	:ylabel "Preference Weight"
	:aspect-ratio 0.2 
	:reset nil))
