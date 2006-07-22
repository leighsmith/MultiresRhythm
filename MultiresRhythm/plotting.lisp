;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for plotting various signals.
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
	(list (hls-value m1 m2 (+ hue 120))   ; R
	      (hls-value m1 m2 hue)	      ; G
	      (hls-value m1 m2 (- hue 120)))  ; B
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

;;; Functions to create IMAGO image instances from magnitude and phase components of a
;;; CWT.

(defun make-colour-mapped-image (matrix-to-plot colour-map)
  "Creates an Imago image instance from the supplied nlisp matrix and colour-map"
  (let ((image (make-instance 'imago:indexed-image
			      :width (.array-dimension matrix-to-plot 0)
			      :height (.array-dimension matrix-to-plot 1)
			      :color-count (length colour-map))))
    (setf (slot-value image 'imago::pixels) (val matrix-to-plot))
    (setf (slot-value image 'imago::colormap) colour-map)
    image))

(defun magnitude-image (magnitude 
			&key (magnitude-limit 0 magnitude-limit-supplied-p)
			(maximum-colour-value 255))
  "Creates a plotable image object from the magnitude using a greyscale colour map.
   Dark values are higher valued, lighter values are lower valued.
   magnitude-limit = Can be used to clamp the global extrema at limits to allow
   interpreting the magnitude density plots for local extrema."
  ;; 0 - maximum-colour-value inclusive:
  (let* ((greyscale (greyscale-colormap (1+ maximum-colour-value))) 
	 (minmag (.min magnitude))
	 ;; A problem can be that the dynamic range of the signal energy can
	 ;; exceed the grey scales, making most of the interesting local maxima
	 ;; barely observable due to the "height" of the global maxima.
	 ;; Therefore we allow clamping the magnitude at a given limit.
	 (maxmag)
	 (mag-range)
	 (plotable-mag))
    (if magnitude-limit-supplied-p
 	;; Replace with clamp-to-bounds?
 	(let ((exceeded (.> magnitude magnitude-limit)))
 	  (setf magnitude (.+ (.* (.not exceeded) magnitude) 
			      (.* exceeded magnitude-limit)))))
    (setf maxmag (.max magnitude))
    (setf mag-range (- maxmag minmag))
    (setf plotable-mag (.floor (.- maximum-colour-value (.* (./ (.- magnitude minmag) mag-range)
							    maximum-colour-value))))
    (make-colour-mapped-image plotable-mag greyscale)))

;; TODO: would be nice to use saturation to indicate magnitude value on the phase plot.
(defun phase-image (phase magnitude
		    &key (maximum-colour-value 255)
		    (magnitude-minimum-for-phase-plot 0.001))
  "Assumes the magnitude value is positive, phase is -pi -> pi.
   Ill-conditioned phase measures due to low magnitude are displayed as white."
  ;; use a colormap which has the lowest value white, for ill-conditioned phase values,
  ;; the rest a spectral distribution from red -> violet.
  (let* ((white (imago:make-color maximum-colour-value maximum-colour-value maximum-colour-value))
	 ;; use maximum-colour-value rather than the number of colours for 1 less.
	 (phase-colormap (concatenate 'vector (vector white) (spectral-colormap maximum-colour-value)))

	 ;; Phase assumed [-pi -> pi], map it to [1 -> maximum-colour-value].
	 ;; When magnitude < magnitude-minimum, set the phase to 0, 
	 ;; the first colormap index (0) is set to be white.
	 (plotable-phase (.floor (.* (.> magnitude magnitude-minimum-for-phase-plot) 
				     (.+ (.* (.+ (./ phase (* 2 pi)) 0.5) 
					     (1- maximum-colour-value)) 1.0)))))
    (make-colour-mapped-image plotable-phase phase-colormap)))

(defmethod tactus-image ((tactus ridge) ridges &key (maximum-colour-value 255))
  "Plot the ridges in greyscale and the computed tactus in red, expected tactus in blue."
  (let* ((tactus-colour (imago:make-color maximum-colour-value 0 0)) ; red is for the ridge.
	 ;; Create a color map that is a greyscale for all values except the topmost which is red.
	 (ridge-colormap (concatenate 'vector (greyscale-colormap maximum-colour-value) (vector tactus-colour)))
	 (max-ridge-colours (1- maximum-colour-value))
	 (min-ridge (.min ridges))
	 (ridge-range (.- (.max ridges) min-ridge))
	 (plotable-ridges (.floor (.- max-ridge-colours 
				      (.* (./ (.- ridges min-ridge) ridge-range) max-ridge-colours)))))
    (loop
       for row-index in (scales tactus)
       and tactus-index = 0 then (1+ tactus-index)
       with start-column = (start-sample tactus)
       do
	 (setf (.aref plotable-ridges row-index (+ start-column tactus-index)) maximum-colour-value))
    (make-colour-mapped-image plotable-ridges ridge-colormap)))

#|
	 (plotable-tactus = zeros(size(ridges))

  rowsToIndent = floor((tactusStartSample) / plotReduction);
  ;; Convert the column indexes into a fortran indexed vector
  rowMajorIndexedTactus = ((find(downSampledTactus) .+ rowsToIndent .- 1) * \
			  rows(downSampledRidges)) .+ downSampledTactus;

  ;; Convert the column indexes into a fortran indexed vector
  fortranIndexedTactus = ((find(tactus) - 1) * \
			  rows(ridges)) + tactus;
  plotableTactus(fortranIndexedTactus) = 1;

  ridges-and-tactus = (.+ (.* plotable-ridges (.not plotable-tactus)) (.* plotable-tactus ))
|#


(defun plot-image (image-generator file-extension data-to-plot
		   &key (title "unnamed")
		   (time-axis-decimation 4)
		   (temp-directory "tmp")
		   (png-type "png"))
   "time-axis-decimation = The amount of downsampling of the data along the translation axis before we plot.
   We could do away with this when we can process the data without excess resource strain,
   but it makes for diagrams which are not so wide, making them easier to view and interpret."
  (let* ((pathname (make-pathname :directory (list :absolute temp-directory) 
				  :name (concatenate 'string title file-extension)
				  :type png-type))
	 ;; Downsample the data 
	 (down-sampled-data (mapcar (lambda (x) (decimate x (list 1 time-axis-decimation))) data-to-plot))
	 (plotable-image (apply image-generator down-sampled-data)))
    (imago:write-png plotable-image pathname)
    plotable-image))

;; (plot-image #'magnitude-image "-magnitude" (list magnitude))
;; (plot-image #'phase-image "-phase" (list phase magnitude))
;; (plot-image #'tactus-image "-tactus" (list ridges tactus))
;; (apply #'plot-image (list #'magnitude-image "-magnitude" (list magnitude) :title "blah")) 

(defun plot-images (image-list &key (title "unnamed"))
  "Plot a number of images as supplied in image-list"
  (mapcar (lambda (x) (apply #'plot-image (append x (list :title title)))) image-list))
 
;;; Creates PNG files of the supplied magnitude and phase components of a continuous
;;; wavelet transform.
(defun plot-cwt (magnitude phase
		 &key (title "unnamed")
		 (time-axis-decimation 4))
  "Function to plot the magnitude and phase components of the result of
   a continuous wavelet transform on a signal."
  (plot-images (list (list #'magnitude-image "-magnitude" (list magnitude))
		     (list #'phase-image "-phase" (list phase magnitude))) :title title))

#|
;; How to plot using nlisp (unfortunately the data transferred between gnuplot and nlisp is too much.

    (palette-defined '((0 "#FF0000")
		       (2 "#00FFFF")
		       (6 "#0000FF")))

    (image plotable-mag nil nil
	   :title title
	   :xlabel "time" :ylabel "dilation scale")))
|#

;; TODO all this can be replaced with:
;; (plot-image #'tactus-image "-tactus" (list computed-tactus ridges))
;; once we have decimate as a method, but this might be problematic defining a method elsewhere.
(defun plot-ridges-and-tactus (ridges computed-tactus &key 
			       (title "unnamed")
			       (expected-tactus nil)
			       (time-axis-decimation 4))
  "Plot the ridges in greyscale and the computed tactus in red, expected tactus in blue."

  (let* ((pathname (make-pathname :directory (list :absolute "tmp") 
				  :name (concatenate 'string title "-tactus")
				  :type "png"))
	 ;; Downsample the data
	 (downSampledRidges (decimate ridges (list 1 time-axis-decimation)))
	 ;; TODO this is wrong, the tactus is a object, not a nlisp vector.
	 ;; Perhaps create a decimate method specialised on tactus?
	 (downSampledTactus (decimate-ridge computed-tactus (list 1 time-axis-decimation)))
	 (plotable-image (tactus-image downSampledTactus downSampledRidges)))
    (imago:write-png plotable-image pathname)
    plotable-image))

#|
;;;
;;; Plot locations of original beats, computed claps, the foot tap
;;; amplitude modulation/phase and reference expected clap points.
;;;
(defun plotClaps (comment signalDescription signal claps expectedClaps footTapAM)
  ;; global promptStream;
  ;; fprintf(promptStream, 
  ;;	  ["Press a key for the clap points ", comment, " of ", signalDescription, "\n"]);
  ;; pause
  figure(4);
  title(["Computed foot-tap and expected foot-tap ", comment, " of ", signalDescription]);
  xlabel('Time')
  ylabel('Normalised Intensity')

  if(length(claps(1,:)) != 0)
    if(nargin < 5)
      plot(signal "m^;Original Rhythm;", claps(1,:), claps(2,:), "b^;Computed Claps;");
    elseif(nargin < 6)
      plot(signal, "m^;Original Rhythm;", claps(1,:), claps(2,:), "b^;Computed Claps;",\
	   expectedClaps(1,:), expectedClaps(2,:), "g^;Expected Claps;");
    else
      ylabel('Scaled Intensity/Phase')
      plot(signal, "m^;Original Rhythm;", \
  	   claps(1,:), 2 * claps(2,:), "b^;Computed Claps;", \
  	   expectedClaps(1,:), 3 * expectedClaps(2,:), "g^;Expected Claps;", \
  	   footTapAM.', "r.;FootTap AM;");
    )
  )
)
|#
