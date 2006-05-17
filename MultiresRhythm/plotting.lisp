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

TODO:would be nice to use saturation to indicate magnitude value
on the phase plot.
"


  ;; Downsample the data 
  (let* ((down-sampled-magnitude (decimate magnitude (list 1 time-axis-decimation)))
	 (minmag (.min down-sampled-magnitude))
	 ;; A problem can be that the dynamic range of the signal energy can
	 ;; exceed the grey scales, making most of the interesting local maxima
	 ;; barely observable due to the "height" of the global maxima.
	 ;; Therefore we allow clamping the magnitude at a given limit.
	 (maxmag (.max down-sampled-magnitude))	; what it would have been
	 (mag-range)
	 (maxcolors 256)
	 (plotable-mag))
    (if magnitude-limit-supplied-p
	;; Replace with clamp-to-bounds?
	(let ((exceeded (.> down-sampled-magnitude magnitude-limit)))
	  (setf down-sampled-magnitude (.+ (.* (.not exceeded) down-sampled-magnitude) 
					   (.* exceeded magnitude-limit)))))
    (setf maxmag (.max down-sampled-magnitude))
    (setf mag-range (- maxmag minmag))
    (setf plotable-mag (.- maxcolors (.* (./ (.- down-sampled-magnitude minmag) mag-range) maxcolors)))
;;    namedImage([title, "-magnitude"], plotable-mag, 1)

    (palette-defined '((0 "#FF0000")
		       (2 "#00FFFF")
		       (6 "#0000FF")))

    (image plotable-mag nil nil
	   :title title
	   :xlabel "time" :ylabel "dilation scale")))

#|
  (if phase-supplied-p
    (let* ((
    ;; use a colormap which has the lowest value white, for
    ;; ill-conditioned phase values, the rest a spectral distribution
    ;; red -> violet.
    white = [1.0 1.0 1.0];
    colormap([white; spectral-colormap(maxcolors - 1)]);

    (down-sampled-phase (decimate phase (1 time-axis-decimation))
 
    ;; phase assumed [-pi -> pi], map it to [2 -> maxcolors],
    ;; when down-sampled-magnitude < magnitude-minimum, set the phase to 0, 
    ;; the first colormap index set to be white.
    (plotable-phase (.* (.> down-sampled-magnitude magnitude-minimum) 
  (ceiling ((./ down-sampled-phase pi + 1) / 2) .* (maxcolors - 1))) + 1)
    namedImage([title, "-phase"], plotable-phase, 1)
  )
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
