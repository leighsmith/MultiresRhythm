;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Continuous Wavelet Transform in the Fourier Domain.
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

;; Holds dyadic and unpadded versions of the magnitude and phase output of the CWT.
(defclass scaleogram ()
  ((voices-per-octave 
    :documentation "Frequency resolution as number of scales for a doubling of frequency"
    :initarg :voices-per-octave 
    :accessor voices-per-octave 
    :initform 16)
   (magnitude 
    :documentation "Non-dyadic (trimmed) magnitude component of the complex CWT result"
    :initarg :magnitude 
    :accessor scaleogram-magnitude)
   (phase 
    :documentation "Non-dyadic (trimmed) phase component of the complex CWT result"
    :initarg :phase 
    :accessor scaleogram-phase)
   (dyadic-padded-magnitude 
    :documentation "Dyadic (padded) magnitude component of the complex CWT result"
    :initarg 
    :dyadic-padded-magnitude 
    :accessor dyadic-padded-magnitude)
   (dyadic-padded-phase 
    :documentation "Dyadic (padded) phase component of the complex CWT result"
    :initarg :dyadic-padded-phase 
    :accessor dyadic-padded-phase)
   (time-trim
    :documentation "Parameter to .subarray to trim the padded back to unpadded versions"
    :accessor time-trim)
   ;; TODO should be octaves-analysed-range from skip-initial-octaves to maximum-time-support
   (skip-highest-octaves
    :documentation "Number of octaves of the highest frequencies that have not been analysed for efficiency"
    :initarg :skip-highest-octaves
    :initform 0
    :accessor skip-highest-octaves)))

(defgeneric plot-cwt (scaleogram &key title time-axis-decimation)
  (:documentation "Function to plot the magnitude and phase components of the result of a continuous wavelet transform on a signal."))

(defgeneric plot-cwt+tactus (scaleogram computed-tactus &key title time-axis-decimation)
  (:documentation "Plot the magnitude in greyscale overlaid with the computed tactus in red, the phase overlaid with the tactus in black."))

(defgeneric plot-scale-energy-at-time (scaleogram time)
  (:documentation "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."))

(defgeneric number-of-scales (scaleogram)
  (:documentation "Returns the number of scales that the scaleogram is dilated by."))

(defgeneric number-of-octaves (scaleogram)
  (:documentation "Returns the number of octaves that the scaleogram spans in its dilation."))

(defgeneric label-scale-as-time-support (scaleogram)
  (:documentation "Returns a list of plotting labels giving the time support for each scale"))

;;;; Methods

(defmethod number-of-scales ((scaleogram-to-analyse scaleogram))
  (.row-count (scaleogram-magnitude scaleogram-to-analyse)))

(defmethod number-of-octaves ((scaleogram-to-analyse scaleogram))
  (+ (/ (number-of-scales scaleogram-to-analyse) (voices-per-octave scaleogram-to-analyse))
     (skip-highest-octaves scaleogram-to-analyse)))

(defun gaussian-envelope (width &key (mean 0.0) (stddev 1.0) 
			    (scaling (/ 1d0 (* (sqrt (* 2d0 pi)) stddev))))
  "Compute a gaussian envelope.
   width is the sampling range of an envelope over +/-5 standard deviations.
   mean determines the position of the envelope within the width number of samples.
   stddev determines the standard deviation and therefore the variance of the distribution."
  (let ((x (.rseq -5.0 +5.0 width)))
    (.* scaling
	(.exp (.- (./ (.expt (.- x mean) 2.0) (.* 2d0 stddev stddev)))))))

(defun morlet-wavelet-fourier (signal-time-period wavelet-scale &key (omega0 6.2))
  "Construct a Morlet wavelet filter in the Fourier domain within the length of the signal (so we can multiply).

   wavelet-scale = dilation parameter 'a'
   omega0 = the Internal frequency achieving a minimal DC component, 
   See I. Daubechies ``Ten Lectures on Wavelets'' p76
   We use 6.2 in order to have pulse IOI match their theoretical time support.
   was 5, 5.3364, 6 matches pulse trains with 8 voices per octave."
  ;; create vector of radian frequencies, (0->pi, -pi->0) discretised over the signal-time-period.
  (let* ((half-signal (/ signal-time-period 2))
	 (xi (.concatenate 
	      (.rseq 0 pi half-signal 'n-complex-double-array)
	      (.rseq (- pi) 0 half-signal 'n-complex-double-array)))

	 ;; From Grossmann, Kronland-Martinet and Morlet, we multiply the wavelet-scale
	 ;; by the discretised omega value when scaling in the Fourier domain.
	 (omega  (.* xi wavelet-scale))) ; (plot omega)

  ;; See mallat:tour p76
  ;; According to Holschneider sqrt(2 .* pi) .* 
  ;; check with Daubechies pi ^ (-1/4)
  ;;
  (.- (.exp (.- (./ (.expt (.- omega omega0) 2) 2)))
      (.exp (.- (./ (.+ (.expt omega 2) (.expt omega0 2)) 2))))))

;;; Debugging
;;; (plot (.realpart (morlet-wavelet-fourier 1024 12)) nil)
;;; (setf time-wavelet (ifft (morlet-wavelet-fourier 1024 128)))
;;; (nplot (list (.realpart time-wavelet) (.imagpart time-wavelet)) nil :legends (list "Real Part" "Imaginary Part"))

(defun scale-from-period (time-periods voices-per-octave &key (skip-initial-octaves 2))
  "Function to return a vector of scale numbers from periods of time support (in samples).
   The highest two octaves (time domain extent 0-1,1-2) don't tell us much, so we save
   computation time by skipping them."
  ;; (.* voices-per-octave (.- (.floor (.log time-periods 2)) skip-initial-octaves)))
  (.* voices-per-octave (.- (.log time-periods 2) skip-initial-octaves)))

;;; Given a sampling rate sampRate in Hz, the IOI can be determined in seconds.
(defun time-support (scales wavelets-per-octave &key (skip-initial-octaves 2))
  "Function to return a vector of periods of time support (in samples) from scale numbers.
   The highest two octaves (time domain extent 0-1,1-2) don't tell us much, so we save
   computation time by skipping them."
  (.expt 2d0 (.+ (./ scales (.* wavelets-per-octave 1d0)) skip-initial-octaves)))

;;; Dyadic version assumes the input data is a power of 2.
(defun dyadic-cwt (input-data wavelets-per-octave maximum-time-period 
		   &key (fourier-domain-wavelet #'morlet-wavelet-fourier))
  "Continuous wavelet transform computed in the Fourier domain.
   Returns multiple items (magnitude phase).
   Uses the fourier-domain-wavelet function to give us a scaled version of the wavelet.
   Length of the input-data is assumed to be a power of 2 (dyadic).
   Does the convolution by multiplying in the Fourier domain.
   The maximum-time-period can really only be about 1/4 the input-data to
   still be representing the wavelets in the frequency and time domains meaningfully."
  (let* ((number-of-scales (floor (scale-from-period maximum-time-period wavelets-per-octave)))
	 (time-in-samples (.array-dimension input-data 0))	; time-in-samples is assumed to be a power of 2.
	 ;; This could be very slow to convert to from double-real to complex double
	 (input-data-FFT (fft (.* input-data #C(1d0 0d0))))
	 (magnitude (make-double-array (list number-of-scales time-in-samples)))
	 (phase (make-double-array (list number-of-scales time-in-samples)))

	 ;; Widening period (scale) => increasing time dilation,
	 ;; i.e. lower frequency, narrowing bandwidth in the Fourier domain.
	 (period (time-support (.iseq 1 number-of-scales) wavelets-per-octave)) ; should be double value.

	 ;; Energy normalisation term to keep energy of the scaled wavelets the
	 ;; same as the mother wavelet. Scale the contribution of each voice by
	 ;; its index as the coefficient has a representation 1/sqrt(a) * W(b,a).
	 (voice-scaling (.sqrt period)))
	 ;; voice-scaling = (.* voice-scaling (gauss number-of-scales))
	 
    (format t "Maximum Time Period analysed = ~d, dyadic length = ~d~%" maximum-time-period time-in-samples)

    ;; loop over each voice, beginning with the highest frequency scale.
    (loop 
       for scale-index from 0 below number-of-scales
       do 
	 (let* ((scaled-wavelet (funcall fourier-domain-wavelet time-in-samples (.aref period scale-index)))
		;; Construct the scaled wavelet in the frequency domain. It will be the same length
		;; as the input-data, but mostly zero outside the Gaussian shape.
		;; Multiply in the Fourier domain and take the inverse FFT, thereby producing the convolution.
		(voice-response (ifft (.* input-data-FFT scaled-wavelet (.aref voice-scaling scale-index))))
		;; Produce Magnitude/Phase components from Real/Imaginary values.
		
		;; TODO this is currently incorrectly scaled.
		;; (magnitude-at-scale (./ (.abs voice-response) (.aref voice-scaling scale-index)))
		;; magnitude-at-scale = abs((voice-response .^ 2) ./ (.aref period scale-index)); 
		(magnitude-at-scale (.abs voice-response))

		(scale-row (list scale-index t)))

	   (setf-subarray (val magnitude) (val magnitude-at-scale) scale-row)
	   ;; If the magnitude value is low, the phase will be ill-conditioned,
	   ;; which we will plot different to significant phase behaviour.
	   ;; atan2 used by arg will return -pi to pi.
	   (setf-subarray (val phase) (val (.phase voice-response)) scale-row)))
	   ;; (format t "scale-index ~d~%" scale-index)
    (format t "Finished CWT, last time period = ~d~%" (.aref period (1- number-of-scales)))
    (values magnitude phase)))

;; TODO test with:
;; (dyadic-cwt :fourier-domain-wavelet #'sombrero-wavelet-fourier)

(defun dyadic-length (signal-length)
  "Returns the length of the signal if padded to a dyadic (power of two) length."
  (expt 2 (ceiling (log signal-length 2))))

(defun dyadic-pad (signal &key (silence-pad nil))
  "The latest in signal processing hygene, the soft, comfortable dyadic-pad!
   Protects signals from unsightly window edge conditions! ...sorry couldn't resist :^)

   Instead we actually pad either side of the signal with mirrored portions
   of the signal to a dyadic length to enable efficient FFTs.
   Handles matrices as well as signal vectors, in the former case, assuming the columns
   are to be padded to a dyadic length.

   Returns multiple values pad-signal, trim-dyadic (on time-axis)."
  (let* ((signal-rows (.row-count signal))
	 (matrix-or-vector (if (equalp signal-rows 1) 0 t)) 
	 (signal-length (.length signal))
	 (padded-length (dyadic-length signal-length))
	 (to-pad (- padded-length signal-length)))
    (multiple-value-bind (half-pad off-by-one) (floor to-pad 2)
      (if (zerop to-pad)
	  ;; Redundant case
	  (values signal (list matrix-or-vector (list 0 (1- signal-length))))
	  (values
	   ;; Create the padded signal.
	   (if silence-pad
	       ;; To pad with zeros:
	       (.concatenate (make-double-array (list signal-rows half-pad))
			     signal 
			     (make-double-array (list signal-rows (+ half-pad off-by-one))))
	       ;; Padding with the signal ensures a periodicity of the window.
	       (let* ((last-region (list (- signal-length (+ half-pad off-by-one)) (1- signal-length)))
		      (lastbit (.subarray signal (list matrix-or-vector last-region)))
		      (firstbit (.subarray signal (list matrix-or-vector (list 0 (1- half-pad))))))
		 (.concatenate lastbit signal firstbit)))
	   ;; Create the .subarray trim description.
	   (list matrix-or-vector (list (+ half-pad off-by-one) (- padded-length half-pad 1))))))))

;; (dyadic-pad (.rseq 0 9 10))
;; (dyadic-pad (.rseq2 1 10 10) :silence-pad t)

;;; The public interface to the continuous wavelet transform for non-dyadic signals
(defun cwt (time-signal voices-per-octave 
	    &key (max-wavelet-period (dyadic-length (./ (.array-dimension time-signal 0) 4))))
  "Pads the signal to a dyadic length, then performs the continuous wavelet transform,
   using dyadic-cwt, trimming off the returned result to match the original signal length.
   Returns a scaleogram instance containing magnitude and phase."
  ;; The wavelet transform operates on 1 x N vector
  (format t "Input signal length ~d samples~%" (.array-dimension time-signal 0))
  (multiple-value-bind (pad-signal time-trim) (dyadic-pad time-signal)
    (multiple-value-bind (padded-magnitude padded-phase)
	(dyadic-cwt pad-signal voices-per-octave max-wavelet-period)
      (let* ((scaleogram-result (make-instance 'scaleogram :voices-per-octave voices-per-octave))
	     (scale-rows (.array-dimension padded-magnitude 0))
	     (trim (list (list 0 (1- scale-rows)) (second time-trim))))
	(setf (time-trim scaleogram-result) trim)
	(setf (dyadic-padded-magnitude scaleogram-result) padded-magnitude)
	(setf (dyadic-padded-phase scaleogram-result) padded-phase)
	(setf (scaleogram-magnitude scaleogram-result) (.subarray padded-magnitude trim))
	(setf (scaleogram-phase scaleogram-result) (.subarray padded-phase trim))
	scaleogram-result))))

;;; For inverse CWT.
;;; To achieve conservation of energy between domains, c_g is chosen according to the wavelet.
(defun dyadic-icwt (magnitude phase wavelets-per-octave
		    &key (fourier-domain-wavelet #'morlet-wavelet-fourier) (c_g 1.7))
  "Perform the inverse CWT, reconstructing and the time domain signal from
   the time-frequency domain magnitude and phase components.
   Uses the morlet-wavelet-fourier function to give us a scaled version
   of the wavelet and operations are performed in the Fourier domain.
   Length of the magnitude/phase (the time axis) is assumed to be a power of 2."
  (let* ((time-frequency-dimensions (.array-dimensions magnitude))
	 (number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 ;; we do everything in transpose, so we transpose the convolved result.
	 (time-domain-signal (make-double-array time-in-samples))
	 ;; Reconstruct coefficients as complex numbers from their magnitude and phase components.
	 (complex-coeff (.+ (.* magnitude (.cos phase)) (.* #C(0d0 1d0) magnitude (.sin phase))))
	 ;; for the wavelet generation. TODO check start at 0 or 1?
	 (period (time-support (.iseq 1 number-of-scales) wavelets-per-octave))
	 (voice-scaling (.* c_g (.sqrt period))))

    ;; loop over each "voice", beginning with the highest frequency scale.
    (loop 
       for scale-index from 0 below number-of-scales
       do (let* ((coeff-scale-FFT (fft (.row complex-coeff scale-index))) ; Convert to Fourier domain.
		 (scaled-wavelet (funcall fourier-domain-wavelet time-in-samples (.aref period scale-index)))
	       
		 ;; multiply coefficients for each scale with each of the Fourier domain
		 ;; dilated wavelets and divide by the scale.
		 (convolved (./ (ifft (.* coeff-scale-FFT scaled-wavelet))
				(.aref voice-scaling scale-index))))

	    ;; Sum each scales contributions. The real portion is the original signal, the
	    ;; imaginary is the signal 90 degrees out of phase. This also allows the phase
	    ;; to be extracted.
	    (setf time-domain-signal (.+ time-domain-signal convolved))))
    time-domain-signal))

;;; non-dyadic version
(defun icwt (magnitude phase wavelets-per-octave &rest parameters)
  "Performs the inverse CWT on the given magnitude and phase values. If the magnitude and
   phase components are not dyadic length (power of 2), pads them to a dyadic length
   before performing the inverse cwt (using dyadic-icwt)."
  ;; pad the magnitude and phase 
  (multiple-value-bind (padded-magnitude time-trim) (dyadic-pad magnitude)
    (let ((padded-phase (dyadic-pad phase)))

      ;; To be completely correct the original padded output from dyadic-cwt should be used
      ;; for input to the dyadic-icwt, but the effect is subtle and it's a pain to use globals or hand
      ;; around a padded version of the CWT output.
      ;; global padded-magnitude
      ;; global padded-phase

      ;; Returns the signal and it's Hilbert transform.
      (.subarray (apply #'dyadic-icwt padded-magnitude padded-phase wavelets-per-octave parameters)
		 (list 0 (second time-trim))))))

;;;; Plotting methods

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

;; We reverse the labels so we plot in more intuitive lowest scale on the left orientation.
(defmethod label-scale-as-time-support ((scaleogram-to-plot scaleogram))
  "Generates a set of labels of the scales as time support interval"
  (let* ((vpo (voices-per-octave scaleogram-to-plot))
	 (scale-number-per-octave (.* (.iseq 0 (number-of-octaves scaleogram-to-plot)) vpo))
	 (time-support-per-octave (.floor (time-support scale-number-per-octave vpo))))
    (loop 
       for label across (val (.reverse time-support-per-octave))
       for value across (val scale-number-per-octave)
       collect (list label value)))) ; Should return label as a string.

(defmethod plot-scale-energy-at-time ((scaleogram-to-plot scaleogram) time)
  "Plot a cross-section of the magnitude at a given time point so we can spot the highest activated scales."
  (plot-command "set title font \"Times,20\"")
  (plot-command "set xlabel font \"Times,20\"")
  (plot-command "set ylabel font \"Times,20\"")
  (plot-command "set key off")
  ;; (plot-command "set xtics (\"abc\" 1, \"def\" 20, \"ggg\" 30)")
  (plot-command (format nil "set xtics (~{~{\"~a\" ~d~}~^, ~})~%" (label-scale-as-time-support scaleogram-to-plot)))
  ;; We reverse the column so we plot in more intuitive lowest scale on the left orientation.
  (plot (.reverse (.column (scaleogram-magnitude scaleogram-to-plot) time)) nil 
	:title (format nil "Energy profile at sample number ~d" time)
	:xlabel "Scale as IOI Range in Samples"
	:ylabel "Magnitude Energy"
	:reset nil
	:aspect-ratio 0.2))
