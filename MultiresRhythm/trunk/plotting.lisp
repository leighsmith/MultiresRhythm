;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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

(defun invert-and-scale (matrix maximum-value)
  "Scales a matrix of values ranging (0 to 1) to integers ranging (maximum-value to 0)"
  (.floor (.- maximum-value (.* matrix maximum-value))))

;;; Colour map generating functions

(defun greyscale-colormap (map-length &key (alpha 255))
  (declare (type fixnum map-length))
  (loop for map-index below map-length
     with color-map = (make-array map-length)
     with color-increment = (/ 256 map-length) ; color map colors range from 0 - 255
     with grey-value
     do 
       (setf grey-value (floor (* map-index color-increment)))
       (setf (aref color-map map-index) (imago:make-color grey-value grey-value grey-value alpha))
     finally (return color-map)))

(defun hls-value (n1 n2 hue)
  "Sub-function to convert from hue (0-360 degrees) and positions within the 
   colour pyramid to a value usable for conversion to RGB. From Van Dam and Foley (1982) p619."
    (setf hue (cond ((> hue 360) (- hue 360))
		    ((< hue 0) (+ hue 360))
		    (t hue)))
    (cond ((< hue 60) (+ n1 (* (- n2 n1) (/ hue 60))))
	  ((< hue 180) n2)
	  ((< hue 240) (+ n1 (* (- n2 n1) (/ (- 240 hue) 60))))
	  (t n1)))

(defun hls-to-rgb (hue lightness saturation)
  "Converts from hue (0-360 degrees) lightness (0-1) and saturation (0-1) to normalised RGB values. 
   From Van Dam and Foley (1982) p617."
  ;; assume lightness > 0.5
  (let* ((m2 (- (+ lightness saturation) (* lightness saturation)))
	 (m1 (- (* 2 lightness) m2)))
    (if (not (= saturation 0))
	(list (hls-value m1 m2 (+ hue 120))   ; Red
	      (hls-value m1 m2 hue)	      ; Green
	      (hls-value m1 m2 (- hue 120)))  ; Blue
	(list 0 0 0))))

(defun spectral-colormap (map-length &key (alpha 255))
  "Create a colormap of length map-length which is a spectral distribution, the colors forming a hue wheel."
  (declare (type fixnum map-length))
  (loop for map-index below map-length
     with light = 0.5
     with saturation = 1.0
     with color-map = (make-array map-length)
     with hue-polar-increment = (/ 360 map-length)
     with normalised-rgb ; RGB values 0-1.
     with rgba ; 0-255, including alpha channel
     do 
       (setf normalised-rgb (hls-to-rgb (* map-index hue-polar-increment) light saturation))
       (setf rgba (append (mapcar (lambda (x) (floor (* x 255))) normalised-rgb) (list alpha)))
       (setf (aref color-map map-index) (apply #'imago:make-color rgba))
     finally (return color-map)))

;; (defun search-sorted (a x)
;;   "Returns index of the first element in the array a which exceeds x, and returns the
;;    length of a (the highest index of the array plus 1) if x is larger than the last element.
;;    Assumes that the data in the array is sorted in ascending order."
;;   ;; TODO needs to return an array, this would be ok if list-2-array generated
;;   ;; n-fixnum-arrays and it wasn't as memory hungry.
;;   (nlisp::list-2-array 
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
    (make-instance 'n-fixnum-array :ival (adjust-array found-indexes (fill-pointer found-indexes)))))

;; Only good for vectors, of course.
(defun .subseq (a start &optional end)
  (make-instance (class-of a) :ival (subseq (val a) start end)))

(defun linear-segmented-channel (map-length channel-linear-segments)
  "Create an 1-d lookup table with map-length elements.
   Each element in channel-linear-segments should be a list of x, y0, y1 tuples."
  (let* ((x  (.* (nlisp::list-2-array (mapcar #'first channel-linear-segments)) map-length))
	 (y0 (nlisp::list-2-array (mapcar #'second channel-linear-segments)))
	 ;; (y1 (nlisp::list-2-array (mapcar #'third channel-linear-segments)))
	 (lut-index (.rseq 0.0 map-length (1+ map-length)))	; discretise the look up table.
	 ;; Compute which indexes mark the start of each linear segment.
	 (segment-bases (.- (.subseq (search-sorted x lut-index) 1) 1)))
    ;; Compute the linear interpolation y = mx + b.
    (.+ (.* (./ (.- (.subseq lut-index 1) (.arefs x segment-bases)) 
		(.arefs (diff x) segment-bases)) 
	    (.arefs (diff y0) segment-bases))
	(.arefs y0 segment-bases))))

;; (setf red (linear-segmented-channel 256 '((0.0d0 0.0d0 0.0d0) (0.35d0  0.0d0 0.0d0) (0.66d0  1.0d0 1.0d0) (0.89d0 1.0d0 1.0d0) (1.0d0  0.5d0 0.5d0))))

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
       #'imago:make-color 
       (val (.floor (.* (linear-segmented-channel map-length (first colour-linear-segments)) max-resolution)))
       (val (.floor (.* (linear-segmented-channel map-length (second colour-linear-segments)) max-resolution)))
       (val (.floor (.* (linear-segmented-channel map-length (third colour-linear-segments)) max-resolution)))
       (make-array map-length :initial-element alpha)))

;;; all y0 y1 values are the same.
(defun jet-colormap (map-length)
  "Returns a colour map that transitions from blue, cyan, yellow, orange, red, simulating the colour of a heat jet."
  (linear-segmented-colormap map-length
	'(((0.0d0 0.5d0 0.5d0) (0.11d0 1.0d0 1.0d0) (0.34d0 1.0d0 1.0d0) (0.65d0 0.0d0 0.0d0) (1.0d0 0.0d0 0.0d0))
	  ((0.0d0 0.0d0 0.0d0) (0.09d0 0.0d0 0.0d0) (0.36d0 1.0d0 1.0d0) (0.625d0 1.0d0 1.0d0) (0.875d0 0.0d0 0.0d0) (1.0d0 0.0d0 0.0d0))
	  ((0.0d0 0.0d0 0.0d0) (0.35d0 0.0d0 0.0d0) (0.66d0 1.0d0 1.0d0) (0.89d0 1.0d0 1.0d0) (1.0d0 0.5d0 0.5d0)))))

(defun colour-swatch (colormap &key (swatch-width 30))
  "Make a nice little vertical bar plotting the color map"
  (make-colour-mapped-image (.subarray (.iseq2 0 (1- (length colormap))) (list t (list 0 (1- swatch-width))))
                            colormap))

;; (imago:write-pnm (colour-swatch (spectral-colormap 256) :swatch-width 30) "/tmp/colour-swatch2.pnm" :ascii)
;; (imago:write-pnm (colour-swatch (jet-colormap 256) :swatch-width 30) "/tmp/colour-swatch2.pnm" :ascii)

;;; Functions to create IMAGO image instances from magnitude and phase components of a CWT.

(defun make-colour-mapped-image (matrix-to-plot colour-map)
  "Creates an Imago image instance from the supplied nlisp matrix and colour-map. 
   The maximum values in the matrix and the length of the colour map must match."
  (let ((image (make-instance 'imago:indexed-image
			      :width (.array-dimension matrix-to-plot 0)
			      :height (.array-dimension matrix-to-plot 1)
			      :color-count (length colour-map))))
    (setf (slot-value image 'imago::pixels) (val matrix-to-plot))
    (setf (slot-value image 'imago::colormap) colour-map)
    image))

;; Define this as a parameter rather than as a keyword parameter since it needs to be set
;; several calls higher in the hierarchy - cost of functional programming it seems...
;; (defparameter *magnitude-colour-map* #'jet-colormap)
(defparameter *magnitude-colour-map* #'greyscale-colormap)

(defun magnitude-image (magnitude 
			&key (magnitude-limit 0 magnitude-limit-supplied-p)
			(maximum-colour-value 255))
  "Creates a plotable image object from the magnitude using a colour map.
   Dark values are higher valued, lighter values are lower valued.
   magnitude-limit = Can be used to clamp the global extrema at limits to allow
   interpreting the magnitude density plots for local extrema."
  ;; A problem can be that the dynamic range of the signal energy can
  ;; exceed the grey scales, making most of the interesting local maxima
  ;; barely observable due to the "height" of the global maxima.
  ;; Therefore we allow clamping the magnitude at a given limit.
  (if magnitude-limit-supplied-p
      ;; Replace with clamp-to-bounds?
      (let ((exceeded (.> magnitude magnitude-limit)))
	(setf magnitude (.+ (.* (.not exceeded) magnitude) 
			    (.* exceeded magnitude-limit)))))
  ;; 0 - maximum-colour-value inclusive:
  (let* ((plotable-mag (invert-and-scale (.normalise magnitude) maximum-colour-value)))
    (make-colour-mapped-image plotable-mag (funcall *magnitude-colour-map* (1+ maximum-colour-value)))))

(defun plotable-phase (phase magnitude maximum-colour-value &key (magnitude-minimum-for-phase-plot 0.001))
  "Phase assumed [-pi -> pi], map it to [1 -> maximum-colour-value].
   When magnitude < magnitude-minimum, set the phase to 0."
  (.floor (.* (.> magnitude magnitude-minimum-for-phase-plot) 
	      (.+ (.* (.+ (./ phase (* 2 pi)) 0.5) 
		      (1- maximum-colour-value)) 1.0))))

;; TODO: would be nice to use saturation to indicate magnitude value on the phase plot.
(defun phase-image (phase magnitude &key (maximum-colour-value 255))
  "Assumes the magnitude value is positive, phase is -pi -> pi.
   Ill-conditioned phase measures due to low magnitude are displayed as white."
  ;; use a colormap which has the lowest value white, for ill-conditioned phase values,
  ;; the rest a spectral distribution from red -> violet.
  (let* ((white (imago:make-color maximum-colour-value maximum-colour-value maximum-colour-value))
	 ;; use maximum-colour-value rather than the number of colours for 1 less.
	 (phase-colormap (concatenate 'vector (vector white) (spectral-colormap maximum-colour-value))))
    ;; the first colormap index (0) is set to be white.
    (make-colour-mapped-image (plotable-phase phase magnitude maximum-colour-value) phase-colormap)))

(defmethod tactus-image ((tactus ridge) ridges &key (maximum-colour-value 255))
  "Plot the ridges in greyscale and the computed tactus in red, expected tactus in blue."
  (let* ((tactus-colour (imago:make-color maximum-colour-value 0 0)) ; red is for the ridge.
	 ;; Create a color map that is a greyscale for all values except the topmost which is red.
	 (ridge-colormap (concatenate 'vector (greyscale-colormap maximum-colour-value) (vector tactus-colour)))
	 (max-ridge-colours (1- maximum-colour-value))
	 (plotable-ridges (invert-and-scale (.normalise ridges) max-ridge-colours)))
    (insert-ridge tactus plotable-ridges :constant-value maximum-colour-value)
    (make-colour-mapped-image plotable-ridges ridge-colormap)))

(defun tactus-on-phase-image (tactus phase magnitude &key (maximum-colour-value 255))
  "Plot the phase with the computed tactus in black."
  (let* ((tactus-colour (imago:make-color 0 0 0)) ; black is for the ridge.
	 (white (imago:make-color maximum-colour-value maximum-colour-value maximum-colour-value))
	 ;; Create a color map that is a spectral distribution for all values except the
	 ;; lowest which is white, the topmost which is black for minimal
	 (ridge+phase-colormap (concatenate 'vector 
					    (vector white) 
					    (spectral-colormap (1- maximum-colour-value))
					    (vector tactus-colour)))
	 (max-ridge-colours (1- maximum-colour-value))
	 (plotable-phase-with-ridge (plotable-phase phase magnitude max-ridge-colours)))
    (insert-ridge tactus plotable-phase-with-ridge :constant-value maximum-colour-value)
    (make-colour-mapped-image plotable-phase-with-ridge ridge+phase-colormap)))

(defun plot-image (image-generator file-extension data-to-plot
		   &key (title "unnamed")
		   (time-axis-decimation 4)
		   (temp-directory "tmp")
		   (image-file-type "pnm")) ; Can be "png"
   "Generates a displayable and file writable image from a given set of data to plot.
    time-axis-decimation = The amount of downsampling of the data along the translation axis before we plot.
    We could do away with this when we can process the data without excess resource strain,
    but it makes for diagrams which are not so wide, making them easier to view and interpret."
  (let* ((pathname (make-pathname :directory (list :absolute temp-directory) 
				  :name (concatenate 'string title file-extension)
				  :type image-file-type))
	 ;; Downsample the data 
	 (down-sampled-data (mapcar (lambda (x) (decimate x (list 1 time-axis-decimation))) data-to-plot))
	 (plotable-image (apply image-generator down-sampled-data)))
    ;; (imago:write-png plotable-image pathname)
    (imago:write-pnm plotable-image pathname :ascii)
    plotable-image))

;; (plot-image #'magnitude-image "-magnitude" (list magnitude))
;; (plot-image #'phase-image "-phase" (list phase magnitude))
;; (plot-image #'tactus-image "-tactus" (list ridges tactus))
;; (apply #'plot-image (list #'magnitude-image "-magnitude" (list magnitude) :title "blah")) 

(defun plot-images (image-list &key (title "unnamed") (time-axis-decimation 4))
  "Plot a number of images as supplied in image-list"
  (mapcar 
   (lambda (x) (apply #'plot-image (append x (list :title title :time-axis-decimation time-axis-decimation))))
   image-list))

(defun plot-ridges+tactus (ridges computed-tactus &key 
			       (title "unnamed")
			       (image-extension "-tactus")
			       (time-axis-decimation 4))
  "Plot the ridges in greyscale and the computed tactus in red."
  ;; We make a copy of the tactus since decimate modifies the object (it is, after all, an
  ;; instance method)
  (plot-image #'tactus-image image-extension (list (copy-object computed-tactus) ridges)
	      :title title
	      :time-axis-decimation time-axis-decimation))

(defun plot-claps (rhythm-signal claps &key foot-tap-AM (max-computed-scale 2d0)
		   (comment "") 
		   (signal-description ""))
  "Plot locations of original beats, computed claps, the foot tap
   amplitude modulation/phase and reference expected clap points."
  (let* ((clap-signal (make-double-array (.array-dimensions rhythm-signal) :initial-element 0d0)))
    (map nil (lambda (index) (setf (.aref clap-signal index) max-computed-scale)) (val claps))
    (nplot (list rhythm-signal clap-signal foot-tap-AM) nil
	   :legends '("Original Rhythm" "Computed Claps" "Foot-tap AM")
	   :styles '("impulses linetype 6 linewidth 3" "impulses linetype 4" "dots 3")
	   :xlabel "Time"
	   :ylabel "Scaled Intensity/Phase"
	   :title (format nil "Computed foot-tap ~a of ~a" comment signal-description))))

;; Alternative plot method if not using nplot.
;;  (let ((clap-intensity (make-double-array (.array-dimensions claps) :initial-element 2d0)))
;;  (plot clap-intensity claps :style "impulses"))

;;; How to plot using nlisp now the image function is fixed.
(defun nlisp-image (magnitude &key (title "magnitude") (time-axis-decimation 4))
  (nlisp::palette-defined '((0 "#FF0000")
		     (2 "#00FFFF")
		     (6 "#0000FF")))
  ;; Need to label the plot of the scale axis with cwt label-scale-as-time-support
  (image (decimate magnitude (list 1 time-axis-decimation)) nil nil
	 :title title
	 :xlabel "time" :ylabel "dilation scale"
	 :square nil))

 
