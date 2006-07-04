;;;; -*- Lisp -*-
;;;;
;;;; $Id: plotCWT.m 4714 2006-03-21 10:17:35Z leigh $
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

;; (require 'zlib)
;; (require 'imago)
;; (use-package 'imago)

;; (setf mk-icon-image (imago:read-image "/Library/Documentation/MusicKit/Images/MK_icon.png"))
;; (write-png mk-icon-image "/Users/leigh/mk-icon-new-1.png")

;; (defun set-spectrum ((image indexed-image) &key (alpha 255))
;;   (with-slots (colormap) image
;;     (loop for i below (length colormap)
;;           do (multiple-value-bind (r g b)
;;                  (color-rgb (aref colormap i))
;;                (setf (aref colormap i) (make-color r g b alpha))))))

(defun greyscale-colormap (map-length &key (alpha 255))
  (declare (type fixnum map-length))
  (loop for map-index below map-length
     with color-map = (make-array map-length)
     with color-increment = (/ 256 map-length) ; color map colors range from 0 - 255
     with color-value
     do 
       (setf color-value (floor (* map-index color-increment)))
       (setf (aref color-map map-index) (imago:make-color color-value color-value color-value alpha))
     finally (return color-map)))

#|
;;; From Van Dam and Foley (1982) p619.
(defun hls-value (n1 n2 hue)
"Sub-function to convert from hue (0-360 degrees) and positions within the 
 colour pyramid to a value usable for conversion to RGB."

  for i = 1:length(hue)
    if hue(i) > 360
      hue(i) = hue(i) - 360;
    endif
    if hue(i) < 0
      hue(i) = hue(i) + 360;
    endif
    if hue(i) < 60
      value(i) = n1(i) + (n2(i) - n1(i)) .* hue(i) / 60;
    elseif hue(i) < 180
      value(i) = n2(i);
    elseif hue(i) < 240
      value(i) = n1(i) + (n2(i) - n1(i)) .* (240 - hue(i)) / 60;
    else
      value(i) = n1(i);
    endif
  endfor
value)

(defun hls-to-rgb (hue lightness saturation)
  "Converts from hue (0-360 degrees) lightness (0-1) and saturation (0-1) to RGB values. 
   From Van Dam and Foley (1982) p617."
  ;; assume lightness > 0.5
  (let* ((m2 (.- (.+ lightness saturation) (.* lightness saturation)))
	 (m1 (.- (.* 2 lightness) m2))

  (if (saturation != 0)
    r = (hls-value m1 m2 (+ hue 120))
    g = (hls-value m1 m2 hue)
    b = (hls-value m1 m2 (- hue 120))
  )
  rgb = [r.', g.', b.'];
)
|#

(defun spectral-colormap (&optional (n 64))
  "Create a colormap of length n which is a spectral distribution, the colors forming a hue wheel."
  (let ((light (make-array n :initial-element 0.5))
	(saturation (make-array n :initial-element 1.0)))
    (hls-to-rgb (.rseq 1 360 n) light saturation)))

(defun magnitude-image (magnitude 
			&key (magnitude-limit 0 magnitude-limit-supplied-p)
			(maximum-colour-value 255))
  "Creates a plotable image object from the magnitude using a greyscale colour map"
  ;; 0 - maximum-colour-value inclusive:
  (let* ((greyscale (greyscale-colormap (1+ maximum-colour-value))) 
	 (minmag (.min magnitude))
	 ;; A problem can be that the dynamic range of the signal energy can
	 ;; exceed the grey scales, making most of the interesting local maxima
	 ;; barely observable due to the "height" of the global maxima.
	 ;; Therefore we allow clamping the magnitude at a given limit.
	 (maxmag)
	 (mag-range)
	 (plotable-mag)
	 (plotable-mag-image))
    (if magnitude-limit-supplied-p
 	;; Replace with clamp-to-bounds?
 	(let ((exceeded (.> magnitude magnitude-limit)))
 	  (setf magnitude (.+ (.* (.not exceeded) magnitude) 
			      (.* exceeded magnitude-limit)))))
    (setf maxmag (.max magnitude))
    (setf mag-range (- maxmag minmag))
    (setf plotable-mag (.floor (.- maximum-colour-value (.* (./ (.- magnitude minmag) mag-range)
							    maximum-colour-value))))

    (setf plotable-mag-image (make-instance 'imago:indexed-image
 					    :width (.array-dimension plotable-mag 0)
 					    :height (.array-dimension plotable-mag 1)
 					    :color-count (1+ maximum-colour-value)))
    (setf (slot-value plotable-mag-image 'imago::pixels) (val plotable-mag))
    (setf (slot-value plotable-mag-image 'imago::colormap) greyscale)
    plotable-mag-image))

(defun phase-image (phase magnitude
		    &key (maximum-colour-value 255)
		    (magnitude-minimum-for-phase-plot 0.001))
  ;; use a colormap which has the lowest value white, for ill-conditioned phase values,
  ;; the rest a spectral distribution from red -> violet.
  (let* ((white (imago:make-color maximum-colour-value maximum-colour-value maximum-colour-value))
	 ;; use maximum-colour-value rather than the number of colours for 1 less.
	 (phase-colormap (concatenate 'array white (spectral-colormap maximum-colour-value)))

	 ;; Phase assumed [-pi -> pi], map it to [1 -> maximum-colour-value].
	 ;; When magnitude < magnitude-minimum, set the phase to 0, 
	 ;; the first colormap index (0) is set to be white.
	 (plotable-phase (.floor (.* (.> magnitude magnitude-minimum-for-phase-plot) 
				     (.+ (.* (.+ (./ phase (* 2 pi)) 0.5) 
					     (1- maximum-colour-value)) 1.0))))
	 (plotable-phase-image (make-instance 'imago:indexed-image
					      :width (.array-dimension plotable-phase 0)
					      :height (.array-dimension plotable-phase 1)
					      :color-count (1+ maximum-colour-value))))
    (setf (slot-value plotable-phase-image 'imago::pixels) (val plotable-phase))
    (setf (slot-value plotable-phase-image 'imago::colormap) phase-colormap)
    plotable-phase-image))

;; Split up into phase-image functions
(defun plot-cwt (magnitude &optional (phase nil phase-supplied-p)
		&key (title "" title-supplied-p)
		(magnitude-limit 0 magnitude-limit-supplied-p)
		(magnitude-minimum 0.001) 
		(time-axis-decimation 4))
  "Function to plot the magnitude and phase components of the result of
a continuous wavelet transform on a signal.

Assumes the magnitude value is positive, phase is -pi -> pi.
Ill-conditioned phase measures due to low magnitude are displayed as white.

magnitude-limit = Can be used to clamp the global extrema at limits to allow
interpreting the magnitude density plots for local extrema.

 time-axis-decimation = The amount of downsampling of the cwt result on the translation
 axis we do before we plot.
 We could do away with this when we can process the data without excess resource strain,
 but it makes for diagrams which are not so wide, making them easier to view and interpret.

TODO:would be nice to use saturation to indicate magnitude value on the phase plot.
"
  (let* ((filename "/Users/leigh/empty-image.png")
	 ;; Downsample the data 
	 (down-sampled-magnitude (decimate magnitude (list 1 time-axis-decimation)))
	 (plotable-mag-image (magnitude-image down-sampled-magnitude)))
    ;; [title, "-magnitude"]
    (imago:write-png plotable-mag-image filename)
    (if phase-supplied-p
	(let* ((plotable-phase-image (phase-image (decimate phase (1 time-axis-decimation))
						  down-sampled-magnitude)))
	  ;; "-phase"
	  (imago:write-png plotable-phase-image filename))))
  plotable-mag-image)

;; (with-slots (imago::colormap) empty-image (setf (aref imago::colormap 255) (make-color 255 255 255)))
;; (with-slots (imago::colormap) empty-image (setf (aref imago::colormap 0) (make-color 0 255 255)))
;; (image-colormap empty-image)
;; (setf (image-pixel empty-image 25 50) 255)

#|
    (palette-defined '((0 "#FF0000")
		       (2 "#00FFFF")
		       (6 "#0000FF")))

    (image plotable-mag nil nil
	   :title title
	   :xlabel "time" :ylabel "dilation scale")))
|#


#|
;;;
;;; Plot the ridges in greyscale and the computed tactus in red, 
;;; expected tactus in blue.
;;;
(defun plotRidgesAndTactus (comment ridges computedTactus expectedTactus)
  magnitude-minimum = 0.001;
  ;; The amount of downsampling of the cwt result on the translation axis
  ;; we do before we plot.
  ;; We could do away with this since we can process the data without
  ;; excess resource strain, but the images usually exceed the width of
  ;; a window, so this is mainly to provide a useful viewing aspect ratio.
  time-axis-decimation = 4;

  ;; Downsample the data 
  downSampledRidges = (decimate ridges (1 time-axis-decimation))
  downSampledTactus = (decimate computedTactus (1 time-axis-decimation))
  plotableTactus = zeros(size(downSampledRidges));

  greyscale = colormap("default");
  maxcolours = length(greyscale);
  maxRidgeColours = maxcolours - 1;

  ;; red is for the ridge.
  red = [1.0 0.0 0.0];

  ;; Create a color map that is a greyscale for all values except the
  ;; topmost which is red.
  ridgeColourMap = [greyscale(1: length(greyscale) - 1, :); red];

  colormap(ridgeColourMap);

  maxridge = max(max(downSampledRidges));
  minridge = min(min(downSampledRidges));
  ridgerange = maxridge - minridge;
  plotableRidges = maxRidgeColours - (((downSampledRidges - minridge) ./ ridgerange) .* maxRidgeColours);

  ;; Convert the column indexes into a fortran indexed vector
  fortranIndexedTactus = ((find(downSampledTactus) - 1) * \
			  rows(downSampledRidges)) + downSampledTactus;
  plotableTactus(fortranIndexedTactus) = 1;

  plotableImage = plotableRidges .* ~plotableTactus + plotableTactus * maxcolours;
  namedImage("tactus", plotableImage, 1);
)

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
