;;;; -*- Lisp -*-
;;;;
;;;; Functions for plotting various signals.
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

;; Controls what we will plot. We can amend this as needed at the REPL with (push 'feature *plotting*)
;; '(claps beat-period tactus)
(defparameter *plotting* '())
(defparameter *colour-box-x* 0.88)
(defparameter *colour-box-y* 0.25)

;;; Define a class that holds the parameters for producing a plot.
(defclass image-plot ()
  ((title :initarg :title :accessor title :initform "")
   (window-dimensions :initarg :window-dimensions :accessor window-dimensions :initform '((1.0 1.0) (0.0 0.0)))
   (palette :initarg :palette :accessor image-palette :initform :greyscale)
   (aspect-ratio :initarg :aspect-ratio :accessor aspect-ratio :initform 0.15)
   (label-font :initarg :label-font :accessor label-font :initform "Times,20")
   (time-units :initarg :time-units :accessor time-units :initform "Seconds")
   (label-time-axis :initarg label-time-axis :accessor label-time-access :initform t)
    ;;; We could do away with this when we can process the data without excess resource strain,
    ;;; but it also makes diagrams which are not so wide, making them easier to view and interpret.
   (time-axis-decimation :initarg :time-axis-decimation :accessor time-axis-decimation :initform 4
			 :documentation "The amount of downsampling of the data along the translation axis before we plot.")))

;;; Declaration of interface

;;; TODO Enable once plot-image is no longer a function
;;;(defgeneric plot-image (image-to-plot data-to-plot)
;;;  (:documentation "Plot images with time axis decimation and labelling for multiresolution"))

;;; Implementation

(defmacro diag-plot (plot-name &body body)
  "Call the functions in the body if the named feature is in the plotting feature list"
  `(cond ((find ,plot-name *plotting*)
	  (window)
	  ,@body
	  (close-window))))

;;; We assume max-undecimated-sample is the length of a scaleogram, *before time axis
;;; decimation* so the last element is the sample after the data to be displayed.
(defun label-samples-as-seconds (max-undecimated-sample sample-rate &key (start 0)
				 (maximum-indices 9) (time-axis-decimation 1))
  "Generates a set of labels of the samples as time in seconds"
  (let* ((sample-index (.rseq start (/ max-undecimated-sample time-axis-decimation) maximum-indices))
	 (time-index (./ (.* sample-index time-axis-decimation) sample-rate)))
    (loop 
       for label across (val time-index)
       for position across (val sample-index)
       collect (list label (floor position)))))

(defun label-samples (max-undecimated-sample &key (start 0)
		      (maximum-indices 9) (time-axis-decimation 1))
  "Generates a set of labels of the samples as time in samples"
  (let* ((sample-index (.rseq start (/ max-undecimated-sample time-axis-decimation) maximum-indices))
	 (time-index (.* sample-index time-axis-decimation)))
    (loop 
       for label across (val time-index)
       for position across (val sample-index)
       collect (list (floor label) (floor position)))))

(defun label-phase-in-radians (phaseogram-range divisions)
  "Returns a list of strings and values to graphically label phase values"
  (declare (ignore phaseogram-range divisions))
  '(("-p" 0) ("-p/2" 64) ("0" 128) ("p/2" 192) ("p" 254)))
  ;; '(("-pi" 0) ("-pi/2" 64) ("0" 128) ("pi/2" 192) ("pi" 254)))

(defun label-phase-in-radians-2 (phaseogram-range divisions)
  "Returns a list of strings and values to graphically label phase values"
  (loop 
     for label-index from 0 upto divisions
     with position-increment = (/ phaseogram-range divisions)
     with label-increment = (/ (* 2 pi) divisions)
     for position = (* label-index position-increment)
     for label = (- (* label-index label-increment) pi)
     collect (list label position)))

;;; Colour map generating functions

;; (defun search-sorted (a x)
;;   "Returns index of the first element in the array a which exceeds x, and returns the
;;    length of a (the highest index of the array plus 1) if x is larger than the last element.
;;    Assumes that the data in the array is sorted in ascending order."
;;   ;; TODO needs to return an array, this would be ok if make-narray generated
;;   ;; n-fixnum-arrays and it wasn't as memory hungry.
;;   (make-narray 
;;    (loop 
;;       for to-find across (val x)
;;       ;; TODO If a is assumed sorted, we could do a binary search etc across the array?
;;       collect (position to-find (val a) :test #'<=))))

(defun search-sorted (a x)
  "Returns index of the first element in the array a which exceeds x, and returns the
   length of a (the highest index of the array plus 1) if x is larger than the last element.
   Assumes that the data in the array is sorted in ascending order."
  (let* ((element-length (.array-total-size x))
	 (found-indexes (make-array element-length :fill-pointer 0))
	 (next-position))
    (dotimes (to-find-index element-length)
      (setf next-position (position (.aref x to-find-index) (val a) :test #'<=))
      (vector-push (if next-position next-position (.length a)) found-indexes))
    ;; TODO this should be able to be done without declaring the type of the array.
    (make-instance 'n-integer-array :ival (adjust-array found-indexes (fill-pointer found-indexes)))))

(defun linear-segmented-channel (map-length channel-linear-segments)
  "Create an 1-d lookup table with map-length elements.
   Each element in channel-linear-segments should be a list of x, y0, y1 tuples."
  (let* ((x  (.* (make-narray (mapcar #'first channel-linear-segments)) map-length))
	 (y0 (make-narray (mapcar #'second channel-linear-segments)))
	 ;; (y1 (make-narray (mapcar #'third channel-linear-segments)))
	 (lut-index (.rseq 0.0 map-length (1+ map-length)))	; discretise the look up table.
	 ;; Compute which indexes mark the start of each linear segment.
	 (segment-bases (.- (.subseq (search-sorted x lut-index) 1) 1)))
    ;; Compute the linear interpolation y = mx + b.
    (.+ (.* (./ (.- (.subseq lut-index 1) (.arefs x segment-bases)) 
		(.arefs (.diff x) segment-bases)) 
	    (.arefs (.diff y0) segment-bases))
	(.arefs y0 segment-bases))))

;; (setf red (linear-segmented-channel 256 '((0.0d0 0.0d0 0.0d0) (0.35d0  0.0d0 0.0d0) (0.66d0  1.0d0 1.0d0) (0.89d0 1.0d0 1.0d0) (1.0d0  0.5d0 0.5d0))))

(defun make-color (r g b &optional (alpha #xff))
  (logior (ash alpha 24) (ash r 16) (ash g 8) b))

(defun linear-segmented-colormap (map-length colour-linear-segments &key (max-resolution 255) (alpha max-resolution))
  "Create an 1-d lookup table with map-length elements.
   colour-linear-segments is a dictionary with a red, green and blue entries. 
   Each entry should be a list of x, y0, y1 tuples.

   Each element in this list represents how a value between 0 and 1 (inclusive)
   represented by x is mapped to a corresponding value between 0 and 1 (inclusive). The
   two values of y are to allow for discontinuous mapping functions (say as might be found
   in a sawtooth) where y0 represents the value of y for values of x <= to that given, and
   y1 is the value to be used for x > than that given). The list must start with x=0, end
   with x=1, and all values of x must be in increasing order. Values between the given
   mapping points are determined by simple linear interpolation."
  (map '(array integer (*))
       #'make-color 
       (val (.floor (.* (linear-segmented-channel map-length (first colour-linear-segments)) max-resolution)))
       (val (.floor (.* (linear-segmented-channel map-length (second colour-linear-segments)) max-resolution)))
       (val (.floor (.* (linear-segmented-channel map-length (third colour-linear-segments)) max-resolution)))
       (make-array map-length :initial-element alpha)))

;;; all y0 y1 values are the same.
(defun jet-colormap (map-length)
  "Returns a colour map that transitions from blue, cyan, yellow, orange, red, simulating the colour of a heat jet."
  (linear-segmented-colormap map-length
	'(((0d0 0d0 0d0) (0.35d0 0d0 0d0) (0.66d0 1d0 1d0) (0.89d0 1d0 1d0) (1d0 0.5d0 0.5d0))
	  ((0d0 0d0 0d0) (0.125d0 0d0 0d0) (0.375d0 1d0 1d0) (0.64d0 1d0 1d0) (0.91d0 0d0 0d0) (1d0 0d0 0d0))
	  ((0d0 0.5d0 0.5d0) (0.11d0 1d0 1d0) (0.34d0 1d0 1d0) (0.65d0 0d0 0d0) (1d0 0d0 0d0)))
	:alpha 0))

(defun colour-palette (colourmap)
  "Returns a list of indexes into a colour map and hex strings in RGB space of the given
colourmap, suitable for use by NLISP's palette-defined function."
  (loop 
     for colour-index from 0 below (length colourmap)
     collect (list colour-index (format nil "#~6,'0x" (aref colourmap colour-index)))))

;; Define this as a parameter rather than as a keyword parameter since it needs to be set
;; several calls higher in the hierarchy - cost of functional programming it seems...
;; (defparameter *magnitude-colour-map* #'jet-colormap)
;; (defparameter *magnitude-colour-map* :greyscale)

#|
(magnitude-limit 0 magnitude-limit-supplied-p)
 magnitude-limit = Can be used to clamp the global extrema at limits to allow
   interpreting the magnitude density plots for local extrema.
  ;; A problem can be that the dynamic range of the signal energy can
  ;; exceed the grey scales, making most of the interesting local maxima
  ;; barely observable due to the "height" of the global maxima.
  ;; Therefore we allow clamping the magnitude at a given limit.
  (if magnitude-limit-supplied-p
      ;; Replace with clamp-to-bounds?
      (let ((exceeded (.> magnitude magnitude-limit)))
	(setf magnitude (.+ (.* (.not exceeded) magnitude) 
			    (.* exceeded magnitude-limit)))))
|#

;;; Image plotting functions.

(defun set-image-dimensions (image-size image-origin)
  (plot-command "set size ~f,~f" (first image-size) (second image-size))
  (plot-command "set origin ~f,~f" (first image-origin) (second image-origin)))

(defun set-axes-labels (axes-commands)
  (plot-command "set xtics font \"Times,10\"")
  (plot-command "set ytics font \"Times,10\"")
  (plot-command axes-commands))

(defun set-colour-box (data-to-plot window-dimensions &key (colorbox-divisions 4.0))
  "Set the location of the colour box and the tics"
  (let ((window-size (first window-dimensions))
	(window-origin (second window-dimensions)))
    ;; Expand the colorbox and only display the given number of tics.
    (plot-command "set format cb \"%4.2f\"")
    (plot-command "set cbtics ~5,2f" (/ (range data-to-plot) colorbox-divisions))
    (plot-command "set colorbox user origin ~f,~f size 0.03,0.2"
		  (+ (first window-origin) (* (first window-size) *colour-box-x*))
		  (+ (second window-origin) (* (second window-size) *colour-box-y*)))))

(defun phase-colour-box (phase-data window-dimensions  &key (colorbox-divisions 4.0))
  (let ((window-origin (second window-dimensions)))
    (plot-command "set cbtics font \"Symbol,18\"") ; So we can plot pi symbols.
    (plot-command "set cbtics (~{~{\"~a\" ~d~}~^, ~})~%" 
		  (label-phase-in-radians (range phase-data) colorbox-divisions))
    (plot-command "set colorbox user origin ~f,~f size 0.03,0.2" 
		  (+ (first window-origin) *colour-box-x*)
		  (+ (second window-origin) 0.15))))

(defun set-plot-palette (palette &key (maximum-colour-value 255))
  (cond ((eq palette :greyscale) ; White thru grey to black for magnitude plots
	 (nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000"))))
	((eq palette :jet)
	 (nlisp:palette-defined (colour-palette (jet-colormap 100))))
	((eq palette :spectral)
	 (nlisp:palette "model HSV maxcolors 256")
	 (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 1 1, ~d 1 1 1)" maximum-colour-value)))
	((eq palette :spectral-highlight)
	 (nlisp:palette "model HSV maxcolors 256")
	 (nlisp:palette (format nil "defined ( 0 0 0 1, 1 0 1 1, ~d 1 1 1, ~d 0 0 1)"
				(1- maximum-colour-value) maximum-colour-value)))
	((eq palette :grey-smooth)
	 (nlisp:palette "model RGB")
	 (nlisp:palette-defined '((0 "#FFFFFF") (0.5 "#000000") (1 "#FFFFFF"))))
	((eq palette :greyscale-highlight) ; White thru grey to black, with red highlight for ridge plots
	 (nlisp:palette-defined (list '(0 "#FFFFFF") 
				      (list (1- maximum-colour-value) "#000000")
				      (list maximum-colour-value "#FF0000"))))
	(t 				; Otherwise assume it's a colourmap function
	 (nlisp:palette-defined (colour-palette (funcall palette 100))))))

;;; TODO should probably become methods of n-double-array. This would allow inheritance of
;;; plotting behaviour.
;;; Each plot consists of these subfunctions:
;;; Setting the location within the window to plot. (passed in including aspect ratio)
;;; Setting the palette to plot with. (determined by the image-plotter)
;;; Plotting the image, with titles (determined by image-plotter) 
;;; Setting the colour box display. (determined by image-plotter and window-position)
(defun magnitude-image (magnitude window-dimensions title xlabel &key
			(palette :greyscale)
			(aspect-ratio 0.15)
			(units "Seconds"))
  "Creates a plotable image object from the magnitude using a colour map.
   Dark values are higher valued, lighter values are lower valued."
  (set-colour-box magnitude window-dimensions)
  (set-plot-palette palette)
  (image (.flip magnitude) nil nil
	 :title title
	 :xlabel xlabel
	 :ylabel (format nil "Scale as IOI Range\\n(~a)" units)
	 :reset nil
	 :aspect-ratio aspect-ratio))

(defun plotable-phase (phase magnitude maximum-colour-value &key (magnitude-minimum-for-phase-plot 0.001))
  "Phase assumed [-pi -> pi], map it to [1 -> maximum-colour-value].
   When magnitude < magnitude-minimum, set the phase to 0."
  (.floor (.* (.> magnitude magnitude-minimum-for-phase-plot) 
	      (.+ (.* (.+ (./ phase (* 2 pi)) 0.5) 
		      (1- maximum-colour-value)) 1.0))))

;; TODO: would be nice to use saturation to indicate magnitude value on the phase plot.
(defun phase-image (phase magnitude window-dimensions title &key
		    (maximum-colour-value 255d0)
		    (aspect-ratio 0.15)
		    (palette :spectral)
		    (units "Seconds"))
  "Assumes the magnitude value is positive, phase is -pi -> pi.
   Ill-conditioned phase measures due to low magnitude are displayed as white."
  (let ((plotable-phase (.* (plotable-phase phase magnitude maximum-colour-value) 1d0)))
    (phase-colour-box plotable-phase window-dimensions)
    (set-plot-palette palette)
    (image (.flip plotable-phase) nil nil
	   :title (format nil "Scaleogram Phase of ~a" title)
	   :xlabel (format nil "Time (~a)" units) 
	   :ylabel (format nil "Scale as IOI Range\\n(~a)" units)
	   :reset nil
	   :aspect-ratio aspect-ratio)))

(defun highlighted-ridges-image (ridges tf-plane window-dimensions title xlabel &key
				 (maximum-colour-value 255d0)
				 (aspect-ratio 0.15)
				 (palette :greyscale-highlight)
				 (units "Seconds"))
  "Plot the TF-plane in greyscale and the ridges in red. Dark values are higher valued, lighter values are lower valued."
  (let* ((max-ridge-colours (1- maximum-colour-value))
	 (plotable-ridges (.* (.normalise tf-plane) max-ridge-colours)))
    (dolist (ridge ridges) 
      (setf plotable-ridges (insert-ridge ridge plotable-ridges :constant-value maximum-colour-value)))
    (set-colour-box plotable-ridges window-dimensions)
    (set-plot-palette palette)
    (image (.flip plotable-ridges) nil nil
	   :title (format nil "Skeleton of ~a" title)
	   :xlabel xlabel
	   :ylabel (format nil "Scale as IOI Range\\n(~a)" units)
	   :reset nil
	   :aspect-ratio aspect-ratio)))

(defun ridges-on-phase-image (ridges phase magnitude window-dimensions title xlabel &key 
			      (maximum-colour-value 255d0)
			      (aspect-ratio 0.15)
			      (palette :spectral-highlight))
  "Plot the phase with the computed tactus in black."
  (let* ((max-ridge-colours (1- maximum-colour-value))
	 (plotable-phase-with-ridges (.* (plotable-phase phase magnitude max-ridge-colours) 1d0)))
    (dolist (ridge ridges) 
      (setf plotable-phase-with-ridges (insert-ridge ridge plotable-phase-with-ridges :constant-value maximum-colour-value)))
    (phase-colour-box plotable-phase-with-ridges window-dimensions)
    (set-plot-palette palette)
    (image (.flip plotable-phase-with-ridges) nil nil
	   :title (format nil "Phase of ~a" title)
	   :xlabel "Time (Seconds)" 
	   :ylabel "Scale as IOI Range\\n(Seconds)"
	   :reset nil
	   :aspect-ratio aspect-ratio)))

(defmethod .decimate ((ridge-list list) reduce-list &rest start-indices)
  "Method to decimate a list of ridges."
  (mapcar (lambda (ridge) (apply #'.decimate ridge reduce-list start-indices)) ridge-list))

#|
;;; TODO to complete. Should become a method of image-plot
(defmethod plot-image ((image-to-plot image-plot) data-to-plot)
   "Plots a displayable image from a given set of data to plot."
   (reset-plot)
   ;; Downsample the data 
   (let* ((down-sampled-data (.decimate data-to-plot (list 1 time-axis-decimation))))
     (plot-command "set title font \"~a\"" (label-font image-to-plot))
     (plot-command "set xlabel font \"~a\"" (label-font image-to-plot))
     (plot-command "set ylabel font \"~a\"" (label-font image-to-plot))
     ;; Modify the window dimensions for the image dimensions to give room for the colour
     ;; bar (when using image, not image-pm3d). 
     (set-image-dimensions (list (* (caar (window-dimensions image-to-plot)) *colour-box-x*) 
				 (cadar (window-dimensions image-to-plot)))
			   (second (window-dimensions image-to-plot)))
     (set-axes-labels (axes-labels image-to-plot))
     (set-colour-box down-sampled-data (window-dimensions image-to-plot))
     (set-plot-palette palette)
     (image (.flip down-sampled-data) nil nil
	    :title (title image-to-plot)
	    :xlabel xlabel
	    :ylabel (format nil "Scale as IOI Range\\n(~a)" (time-units image-to-plot))
	    :reset nil
	    :aspect-ratio (aspect-ratio image-to-plot))
     ;; If we need to do something with the image after plotting, here's where...
     (reset-plot)))

|#

;;; Replace with method above.
(defun plot-image (image-plotter data-to-plot window-dimensions axes-labels
		   &key (title "unnamed")
		   (units "Seconds")
		   (time-axis-decimation 4)
		   (xlabel (format nil "Time (~a)" units)))
   "Plots a displayable image from a given set of data to plot.
    time-axis-decimation = The amount of downsampling of the data along the translation axis before we plot.
    We could do away with this when we can process the data without excess resource strain,
    but it also makes diagrams which are not so wide, making them easier to view and interpret."
   (reset-plot)
   ;; Modify the window dimensions for the image dimensions to give room for the colour
   ;; bar (when using image, not image-pm3d). 
   (set-image-dimensions (list (* (caar window-dimensions) *colour-box-x*) (cadar window-dimensions)) 
			 (second window-dimensions))
   (set-axes-labels axes-labels)
   ;; Downsample the data 
   (let* ((down-sampled-data (.decimate data-to-plot (list 1 time-axis-decimation)))
	  (plotable-image (apply image-plotter (append down-sampled-data (list
									  window-dimensions
									  title xlabel)))))
     ;; If we need to do something with the image after plotting, here's where...
     (reset-plot)
     plotable-image))

;; (plot-image #'magnitude-image (list magnitude) '((1.0 0.5) (0.0 0.3)) "" :title "blah")
;; (plot-image #'phase-image (list phase magnitude) '((1.0 0.5) (0.0 0.0)) "")
;; (plot-image #'highted-ridges-image (list ridges magnitude) '((1.0 0.5) (0.0 0.0)) "")
;; (apply #'plot-image (list #'magnitude-image (list magnitude) "" :title "blah")) 

;;; TODO should become a method of magnitude-plot 
(defun plot-magnitude (data &key (title "unnamed") (axes-labels "") (window-dimensions
								     '((1.0 0.5) (0.0
										  0.3))) xlabel)
  "Plots a 2D image in grey scale"
  (plot-image #'magnitude-image (list data) window-dimensions axes-labels :title title
	      :xlabel xlabel))

;;; TODO Should do this as a image-plot-class and multi-plot-class and subclass from those.
(defun open-multiplot ()
  "Plot a number of images in the same window"
  (window)
  (plot-command "set multiplot")
  (plot-command "set title font \"Times,20\"")
  (plot-command "set xlabel font \"Times,20\"")
  (plot-command "set ylabel font \"Times,20\""))

(defun close-multiplot ()
  "End the multiple plotting"
  (plot-command "unset multiplot")
  (reset-plot)
  (close-window))

;;; TODO deprecated for open/close-multiplot
(defun plot-images (image-list &key (title "unnamed") (time-axis-decimation 4))
  "Plot a number of images as supplied in image-list in the same window"
  (open-multiplot)
  (mapcar 
   (lambda (x) (apply #'plot-image (append x (list :title title :time-axis-decimation time-axis-decimation))))
   image-list)
  (close-multiplot))

(defun plot-claps (original-rhythm claps beat-phase &key (signal-name (name original-rhythm)))
  "Plot locations of original beats, computed claps, the foot tap
   amplitude modulation/phase and reference expected clap points."
  (let* ((rhythm-signal (.* (onset-time-signal original-rhythm) 1d0))
	 (bipolar-rhythm (minusp (.min rhythm-signal)))
	 (max-computed-scale (* (.max rhythm-signal) 1.2d0)) ; to make claps visible.
	 (clap-signal (make-double-array (.array-dimensions rhythm-signal) :initial-element 0d0))
	 (scaled-phase (.* (.+ (./ beat-phase pi 2.0d0) (if bipolar-rhythm 0.0d0 0.5d0)) max-computed-scale)))
    (map nil (lambda (index) (setf (.aref clap-signal index) max-computed-scale)) (val claps))
    (window)
    (plot-command "set yrange [~f:~f]" (if (not bipolar-rhythm) 0 (- max-computed-scale)) (* max-computed-scale 1.4))
    (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		  (label-samples-as-seconds (duration-in-samples original-rhythm) (sample-rate original-rhythm)))
    (nplot (list rhythm-signal clap-signal scaled-phase) nil
	   :legends '("Original Rhythm" "Computed Beats" "Normalised Beat Phase")
	   :styles (list (if bipolar-rhythm "lines linetype 6 linewidth 1" "impulses linetype 6 linewidth 3")
			 "impulses linetype 4" "dots 3")
	   :xlabel "Time in Seconds"
	   :ylabel "Scaled Intensity/Phase"
	   :title (format nil "Computed beat track of ~a" signal-name)
	   :reset nil
	   :aspect-ratio 0.66)
    (close-window)))

;; Alternative plot method if not using plot.
;;  (let ((clap-intensity (make-double-array (.array-dimensions claps) :initial-element 2d0)))
;;  (plot clap-intensity claps :styles '("impulses")))

(defgeneric make-plotable-ridges (tf-plane ridges &key time-axis-decimation maximum-colour-value)
  (:documentation "Downsamples the time-frequency plane and inserts the list of ridges into the plane"))

(defun invert-and-scale (matrix maximum-value)
  "Scales a matrix of values ranging (0 to 1) to integers ranging (maximum-value to 0)"
  (.floor (.- maximum-value (.* matrix maximum-value))))

(defmethod make-plotable-ridges ((tf-plane n-double-array) (ridges list) &key
					(time-axis-decimation 4)
					(maximum-colour-value 255d0))
  "Downsamples the time-frequency plane and inserts the list of ridges into the plane"
  (let* ((max-ridge-colours (1- maximum-colour-value))
	 (downsampled-tf-plane (.decimate tf-plane (list 1 time-axis-decimation)))
	 (plotable-tf-plane (.* (invert-and-scale (.normalise downsampled-tf-plane) max-ridge-colours) 1d0))
	 (downsampled-ridge))
    (dolist (ridge ridges plotable-tf-plane)
      ;; We make a copy of the ridge since decimate modifies the object (it is, after all, an instance method)
      (setf downsampled-ridge (.decimate (copy-object ridge) (list 1 time-axis-decimation)))
      (setf plotable-tf-plane (insert-ridge downsampled-ridge plotable-tf-plane :constant-value maximum-colour-value)))))

(defmethod plot-highlighted-ridges-labelled ((ridges n-double-array) (highlighted-ridges list) &key 
					(title "unnamed")
					(time-axis-decimation 4)
					(aspect-ratio 0.15)
					(colorbox-divisions 4.0)
					(maximum-colour-value 255d0))
  "Plot the ridges encoded in a time-frequency plane in greyscale and the highlighted ridges in red."
  (let* ((max-ridge-colours (1- maximum-colour-value))
    	 (rescaled-ridges (make-plotable-ridges ridges highlighted-ridges
					:time-axis-decimation time-axis-decimation
					:maximum-colour-value maximum-colour-value))
	 (downsampled-ridges-time (.array-dimension rescaled-ridges 1)))
    (window)
    ;; White thru grey to black for ridge plots
    (nlisp:palette-defined (list '(0 "#000000") 
				 (list max-ridge-colours "#FFFFFF")
				 (list maximum-colour-value "#FF0000")))
    (plot-command "set xrange [0:~d]" downsampled-ridges-time)	; ensures last label plotted
    (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		  (label-samples-as-seconds (.array-dimension ridges 1)
					    200
					    ;; (sample-rate original-rhythm)
					    :time-axis-decimation time-axis-decimation))
    ;;    (plot-command "set ytics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
    ;;			  (label-scale-as-time-support-seconds scaleogram-to-plot (sample-rate analysis-rhythm)))
    (plot-command "set cbtics ~5,2f" (/ (range rescaled-ridges) colorbox-divisions))

;; for image x parameter: (.rseq 0.0 (time-as-seconds ridges 200) (.array-dimension rescaled-ridges 1))
    (image (.flip rescaled-ridges) nil nil
 	   :title (format nil "Ridges of ~a" title)
	   :xlabel "Time (Seconds)" 
 	   :ylabel "Scale as IOI Range\\n(Seconds)"
 	   :reset nil
 	   :aspect-ratio aspect-ratio)
    (close-window)))
