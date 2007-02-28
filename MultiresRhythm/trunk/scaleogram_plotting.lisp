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
  (:documentation "Function to plot the magnitude and phase components of the result of a continuous wavelet transform on a signal."))

(defgeneric plot-cwt-labelled (scaleogram &key title time-axis-decimation)
  (:documentation "Function to plot the magnitude and phase components of the result of a continuous wavelet transform on a signal using gnuplot with labelling."))

(defgeneric plot-cwt+tactus-labelled (scaleogram computed-tactus rhythm &key 
						 title time-axis-decimation colorbox-divisions
						 maximum-colour-value aspect-ratio
						 phase-palette magnitude-palette)
  (:documentation "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase overlaid with the tactus in black."))

(defgeneric plot-cwt+tactus (scaleogram computed-tactus &key title time-axis-decimation)
  (:documentation "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase overlaid with the tactus in black."))

(defgeneric plot-scale-energy-at-time (scaleogram time)
  (:documentation "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."))

(defgeneric label-scale-as-time-support (scaleogram)
  (:documentation "Returns a list of plotting labels giving the time support for each scale"))

(defgeneric label-scale-as-time-support-seconds (scaleogram sample-rate)
  (:documentation "Generates a set of labels of the scales as time support intervals in seconds"))

;;;; Methods

;; We reverse the labels so we plot in more intuitive lowest scale on the left orientation.
(defmethod label-scale-as-time-support ((scaleogram-to-plot scaleogram))
  "Generates a set of labels of the scales as time support interval"
  (let* ((vpo (voices-per-octave scaleogram-to-plot))
	 (scale-number-per-octave (.* (.iseq 0 (number-of-octaves scaleogram-to-plot)) vpo))
	 (time-support-per-octave (.floor (time-support scale-number-per-octave vpo))))
    (loop 
       for label across (val (.reverse time-support-per-octave))
       for position across (val scale-number-per-octave)
       collect (list label position)))) ; Should return label as a string.

(defmethod label-scale-as-time-support-seconds ((scaleogram-to-plot scaleogram) sample-rate)
  "Generates a set of labels of the scales as time support intervals in seconds"
  (mapcar (lambda (label-and-index) 
	    (cons (/ (first label-and-index) sample-rate) (rest label-and-index)))
	  (label-scale-as-time-support scaleogram-to-plot)))

(defun label-phase-in-radians (phaseogram-range divisions)
  (declare (ignore phaseogram-range divisions))
  ;; '(("-{/Symbol p}" 0) ("-pi/2" 64) ("0" 128) ("pi/2" 192) ("pi" 254)))
  '(("-pi" 0) ("-pi/2" 64) ("0" 128) ("pi/2" 192) ("pi" 254)))

(defun label-phase-in-radians-2 (phaseogram-range divisions)
  (loop 
     for label-index from 0 upto divisions
     with position-increment = (/ phaseogram-range divisions)
     with label-increment = (/ (* 2 pi) divisions)
     for position = (* label-index position-increment)
     for label = (- (* label-index label-increment) pi)
     collect (list label position)))

;;; Creates standard image files of the supplied magnitude and phase components of a continuous
;;; wavelet transform.
(defmethod plot-cwt ((scaleogram-to-plot scaleogram) &key (title "unnamed") (time-axis-decimation 4))
  "Method to plot the magnitude and phase components of the result of
   a continuous wavelet transform on a signal."
  (plot-images (list (list #'magnitude-image "-magnitude" (list (scaleogram-magnitude scaleogram-to-plot)))
		     (list #'phase-image "-phase" (list (scaleogram-phase scaleogram-to-plot)
							(scaleogram-magnitude scaleogram-to-plot))))
	       :title title
	       :time-axis-decimation time-axis-decimation))

(defun range (matrix)
  (- (.max matrix) (.min matrix)))

;;; TODO see if we can modularise this!!!
;;; Now the image function is fixed, this is how to plot using nlisp with axes labelling.
(defmethod plot-cwt+tactus-labelled ((scaleogram-to-plot scaleogram)
				     (computed-tactus ridge)
				     (analysis-rhythm rhythm) &key 
				     (title "unnamed")
				     (time-axis-decimation 4)
				     (colorbox-divisions 4.0)
				     (maximum-colour-value 255)
				     (magnitude-palette :greyscale)
				     (phase-palette :spectral)
				     (aspect-ratio 0.15))
  "Method to plot the magnitude and phase components of the result of
   a continuous wavelet transform on a signal. Plot the phase with the computed tactus in black."
  (let* ((downsampled-magnitude (.decimate (scaleogram-magnitude scaleogram-to-plot) (list 1 time-axis-decimation)))
	 (downsampled-phase (.decimate (scaleogram-phase scaleogram-to-plot) (list 1 time-axis-decimation)))
	 (downsampled-tactus (.decimate	(copy-object computed-tactus) (list 1 time-axis-decimation)))
	 (downsampled-magnitude-time (.array-dimension downsampled-magnitude 1))
	 ;; (scaleogram-dim-ratio (/ (.array-dimension downsampled-magnitude 0) (.array-dimension downsampled-magnitude 1)))
	 ;; (aspect-ratio (if (< scaleogram-dim-ratio 0.3) 0.15 scaleogram-dim-ratio))
	 (plotable-phase-with-ridge (plotable-phase downsampled-phase downsampled-magnitude maximum-colour-value))
    	 (rescaled-phase (.* (insert-ridge downsampled-tactus plotable-phase-with-ridge :constant-value maximum-colour-value) 1d0)))
    (window)				; put this on a separate window.
    (plot-rhythm-labelled analysis-rhythm)
    ;; Can we move this into plot-cwt-labelled?
    (reset-plot)			; Since we don't reset with image.
    (plot-command "set xtics font \"Times,10\"")
    (plot-command "set ytics font \"Times,10\"")
    (plot-command "set format cb \"%4.2f\"")
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.3")
    ;; Label both magnitude & phase plots in seconds.
    (plot-command (format nil "set xrange [0:~d]" downsampled-magnitude-time))	; ensures last label plotted
    (plot-command (format nil "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
			  (label-samples-as-seconds (duration-in-samples analysis-rhythm)
						    (sample-rate analysis-rhythm)
						    :time-axis-decimation time-axis-decimation)))
    (plot-command (format nil "set ytics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
			  (label-scale-as-time-support-seconds scaleogram-to-plot (sample-rate analysis-rhythm))))
    ;; Expand the colorbox and only display the given number of tics.
    (plot-command "set colorbox user origin 0.88,0.45 size 0.03,0.2")
    (plot-command (format nil "set cbtics ~5,2f" (/ (range downsampled-magnitude) colorbox-divisions)))
    ;; (plot-command "set title -35") ; moves the titles leftwards but it doesn't help our shrinking the size.
    (cond ((eq magnitude-palette :greyscale) ; White thru grey to black for magnitude plots
	   (nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000"))))
	  ((eq magnitude-palette :jet)
	   (nlisp:palette-defined (colour-palette (jet-colormap 100))))
	  (t 				; Otherwise assume it's a colourmap function
	   (nlisp:palette-defined (colour-palette (funcall magnitude-palette 100)))))
    (image (.flip downsampled-magnitude) nil nil
 	   :title (format nil "Magnitude of ~a" title)
	   :xlabel nil
 	   :ylabel "Scale as IOI Range\\n(Seconds)"
 	   :reset nil
 	   :aspect-ratio aspect-ratio)
    ;; Phase plot
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.0")
    (cond ((eq phase-palette :spectral)
	   (nlisp:palette "model HSV maxcolors 256")
	   (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 1 1, ~d 1 1 1, ~d 0 0 1)"
				   (1- maximum-colour-value) maximum-colour-value)))
	  ((eq phase-palette :grey-smooth)
	   (nlisp:palette "model RGB")
	   (nlisp:palette-defined '((0 "#FFFFFF") (0.5 "#000000") (1 "#FFFFFF"))))
	  (t (nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000")))))
    ;; (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 0 1, 1 0 1 1, ~d 1 1 1 )" maximum-colour-value))
    ;; (nlisp:palette (format nil "defined ( 0 0 0 1, 0 0 1 1, 1 0 1 1, ~d 1 1 1 )" 255))
    ;; -1 0 1 0
    ;; (plot-command "set cbtics font \"Symbol,12\"") ; Doesn't work on Aquaterm yet.
    (plot-command (format nil "set cbtics (~{~{\"~a\" ~d~}~^, ~})~%" 
 			  (label-phase-in-radians (range rescaled-phase) colorbox-divisions)))
    (plot-command "set colorbox user origin 0.88,0.15 size 0.03,0.2")
    ;;(format t "maximum of rescaled-phase ~f minimum ~f range ~f~%" (.max rescaled-phase) (.min rescaled-phase) (range rescaled-phase))
    (image (.flip rescaled-phase) nil nil
	   :title (format nil "Phase of ~a" title)
	   :xlabel "Time (Seconds)" 
	   :ylabel "Scale as IOI Range\\n(Seconds)"
	   :reset nil
	   :aspect-ratio aspect-ratio)
    (plot-command "unset multiplot")
    (close-window)
    (reset-plot)))

;;; How to plot using nlisp now the image function is fixed.
(defmethod plot-cwt-labelled ((scaleogram-to-plot scaleogram) &key 
			      (title "unnamed")
			      (time-axis-decimation 4)
			      (colorbox-divisions 4.0)
			      (maximum-colour-value 255)
			      (sample-rate 200)
			      (magnitude-palette :greyscale)
			      (phase-palette :spectral)
			      (aspect-ratio 0.15))
  "Method to plot the magnitude and phase components of the result of
   a continuous wavelet transform on a signal."
  (let* ((downsampled-magnitude (.decimate (scaleogram-magnitude scaleogram-to-plot) (list 1 time-axis-decimation)))
	 (downsampled-phase (.decimate (scaleogram-phase scaleogram-to-plot) (list 1 time-axis-decimation)))
	 (downsampled-magnitude-time (.array-dimension downsampled-magnitude 1))
	 ;; (scaleogram-dim-ratio (/ (.array-dimension downsampled-magnitude 0) (.array-dimension downsampled-magnitude 1)))
	 ;; (aspect-ratio (if (< scaleogram-dim-ratio 0.3) 0.15 scaleogram-dim-ratio))
	 (rescaled-phase (.* (plotable-phase downsampled-phase downsampled-magnitude maximum-colour-value) 1d0))
	 ;; (downsampled-tactus (.decimate	(copy-object computed-tactus) (list 1 time-axis-decimation)))
	 ;; (rescaled-phase-with-ridge (insert-ridge (.* downsampled-tactus 1d0) rescaled-phase :constant-value maximum-colour-value))
	 )
    (reset-plot)			; Since we don't reset with image.
    (plot-command "set multiplot")	; Put the magnitude plot above the phase on the same window.
    (plot-command "set xtics font \"Times,10\"")
    (plot-command "set ytics font \"Times,10\"")
    (plot-command "set format cb \"%4.2f\"")
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.3")
    ;; Label both magnitude & phase plots in seconds.
    (plot-command (format nil "set xrange [0:~d]" downsampled-magnitude-time))	; ensures last label plotted
    (format t "duration in samples ~a~%" (duration-in-samples scaleogram-to-plot))
    (plot-command (format nil "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
			  (label-samples-as-seconds (duration-in-samples scaleogram-to-plot)
						    sample-rate
						    :time-axis-decimation time-axis-decimation)))
    (plot-command (format nil "set ytics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
			  (label-scale-as-time-support-seconds scaleogram-to-plot sample-rate)))
    ;; Expand the colorbox and only display the given number of tics.
    (plot-command "set colorbox user origin 0.88,0.45 size 0.03,0.2")
    (plot-command (format nil "set cbtics ~5,2f" (/ (range downsampled-magnitude) colorbox-divisions)))
    ;; (plot-command "set title -35") ; moves the titles leftwards but it doesn't help our shrinking the size.
    ;; White thru grey to black for magnitude plots
    (cond ((eq magnitude-palette :greyscale)
	   (nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000"))))
	  ((eq magnitude-palette :jet)
	   (nlisp:palette "model HSV")
	   ;; (nlisp:palette "defined ( 0 0 1 1, 1 1 1 1 )")
	   (nlisp:palette-defined '((0.0 "0.666 1.0 1.0") (1.0 "0.0 1.0 1.0")))))


;;	'(((0.0 0.5) (0.11 1.0) (0.34 1.0) (0.65 0.0) (1.0 0.0))
;;	  ((0.0 0.0) (0.09 0.0) (0.36 1.0) (0.625 1.0) (0.875 0.0) (1.0 0.0))
;;	  ((0.0 0.0) (0.35 0.0) (0.66 1.0) (0.89 1.0) (1.0 0.5)))

    (image (.flip downsampled-magnitude) nil nil
 	   :title (format nil "Magnitude of ~a" title)
	   :xlabel nil
 	   :ylabel "Scale as IOI Range\\n(Seconds)"
 	   :reset nil
 	   :aspect-ratio aspect-ratio)
    ;; Phase plot
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.0")
    (cond ((eq phase-palette :spectral)
	   (nlisp:palette "model HSV maxcolors 256")
	   (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 1 1, ~d 1 1 1, ~d 0 0 1)"
				   (1- maximum-colour-value) maximum-colour-value)))
	  ((eq phase-palette :grey-smooth)
	   (nlisp:palette "model RGB")
	   (nlisp:palette-defined '((0 "#FFFFFF") (0.5 "#000000") (1 "#FFFFFF"))))
	  (t (nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000")))))
    ;; (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 0 1, 1 0 1 1, ~d 1 1 1 )" maximum-colour-value))
    ;; (nlisp:palette (format nil "defined ( 0 0 0 1, 0 0 1 1, 1 0 1 1, ~d 1 1 1 )" 255))
    ;; -1 0 1 0
    ;; (plot-command "set cbtics font \"Symbol,12\"") ; Doesn't work on Aquaterm yet.
    (plot-command (format nil "set cbtics (~{~{\"~a\" ~d~}~^, ~})~%" 
 			  (label-phase-in-radians (range rescaled-phase) colorbox-divisions)))
    (plot-command "set colorbox user origin 0.88,0.15 size 0.03,0.2")
    ;;(format t "maximum of rescaled-phase ~f minimum ~f range ~f~%" (.max rescaled-phase) (.min rescaled-phase) (range rescaled-phase))
    (image (.flip rescaled-phase) nil nil
	   :title (format nil "Phase of ~a" title)
	   :xlabel "Time (Seconds)" 
	   :ylabel "Scale as IOI Range\\n(Seconds)"
	   :reset nil
	   :aspect-ratio aspect-ratio)
    (plot-command "unset multiplot")
    (reset-plot)))


#|

    ;; "set size 0.7,0.7"
    ;; "set origin 0.1,0.1"
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.5")
    ;; Label both magnitude & phase plots in seconds.
    (plot-command (format nil "set xtics (~{~{\"~5,3f\" ~5d~}~^, ~})~%" 
			  (label-samples-as-seconds (duration-in-samples scaleogram-to-plot)
						    200.0 ; TODO (sample-rate scaleogram-to-plot)
						    :time-axis-decimation time-axis-decimation)))
    (plot-command (format nil "set ytics (~{~{\"~a\" ~d~}~^, ~})~%" 
			  (label-scale-as-time-support-seconds scaleogram-to-plot 200.0))) ; TODO (sample-rate scaleogram-to-plot)
    ;; Expand the colorbox and only display the given number of tics.
    (plot-command "set colorbox user origin 0.88,0.65 size 0.03,0.2")
    (plot-command (format nil "set cbtics ~f" (/ (range downsampled-magnitude) colorbox-divisions)))
    ;; White thru grey to black for magnitude plots
    (nlisp:palette-defined '((0 "#FFFFFF")
			     (1 "#000000")))
    (image (.flip downsampled-magnitude) nil nil
	   :title (format nil "Magnitude of ~a" title)
	   :xlabel "Time in Seconds" 
	   :ylabel "Scale as IOI Range in Seconds"
	   :reset nil
	   :aspect-ratio 0.15)
    ;; Phase plot
    (plot-command "set size 1.0,0.5")
    (plot-command "set origin 0.0,0.1")
    (nlisp::full-color-hsv)
    (plot-command (format nil "set cbtics ~f" (/ (range rescaled-phase) colorbox-divisions)))
    (plot-command "set colorbox user origin 0.88,0.25 size 0.03,0.2")
    (image (.flip rescaled-phase) nil nil
	   :title (format nil "Phase of ~a" title)
	   :xlabel "Time in Seconds" 
	   :ylabel "Scale as IOI Range in Seconds"
	   :reset nil
	   :aspect-ratio 0.15)
    (plot-command "unset multiplot")
    (reset-plot)))
|#

(defmethod plot-cwt+tactus ((scaleogram-to-plot scaleogram) (computed-tactus ridge)
			     &key (title "unnamed") (time-axis-decimation 4))
  "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase
   overlaid with the tactus in black."
  ;; We make a copy of the tactus since decimate modifies the object (it is, after all, an
  ;; instance method)
  (plot-images (list (list #'tactus-image "-mag+tactus" (list (copy-object computed-tactus)
							      (scaleogram-magnitude scaleogram-to-plot)))
		     (list #'tactus-on-phase-image "-phase+tactus" (list (copy-object computed-tactus)
									 (scaleogram-phase scaleogram-to-plot)
									 (scaleogram-magnitude scaleogram-to-plot))))
	       :title title
	       :time-axis-decimation time-axis-decimation))

(defmethod plot-scale-energy-at-time ((scaleogram-to-plot scaleogram) time)
  "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."
  (plot-command "set title font \"Times,20\"")
  (plot-command "set xlabel font \"Times,20\"")
  (plot-command "set ylabel font \"Times,20\"")
  (plot-command "set key off")
  (plot-command (format nil "set xtics (~{~{\"~a\" ~d~}~^, ~})~%" (label-scale-as-time-support scaleogram-to-plot)))
  ;; We reverse the column so we plot in more intuitive lowest scale on the left orientation.
  (plot (.reverse (.column (scaleogram-magnitude scaleogram-to-plot) time)) nil 
	:title (format nil "Energy profile at sample number ~d" time)
	:xlabel "Scale as IOI Range in Samples"
	:ylabel "Magnitude Energy"
	:reset nil
	:aspect-ratio 0.2))
