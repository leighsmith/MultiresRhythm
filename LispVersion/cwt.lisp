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
    :initarg :dyadic-padded-magnitude 
    :accessor dyadic-padded-magnitude)
   (dyadic-padded-phase 
    :documentation "Dyadic (padded) phase component of the complex CWT result"
    :initarg :dyadic-padded-phase 
    :accessor dyadic-padded-phase)
   (time-trim
    :documentation "Parameter to .subarray to trim the padded back to unpadded versions"
    :initarg :time-trim
    :accessor time-trim)
   ;; TODO should be octaves-analysed-range from skip-initial-octaves to maximum-time-support
   (skip-highest-octaves
    :documentation "Number of octaves of the highest frequencies that have not been analysed for efficiency"
    :initarg :skip-highest-octaves
    :initform 0
    :accessor skip-highest-octaves)))

(defgeneric number-of-scales (scaleogram)
  (:documentation "Returns the number of scales that the scaleogram is dilated by."))

(defgeneric number-of-octaves (scaleogram)
  (:documentation "Returns the number of octaves that the scaleogram spans in its dilation."))

(defgeneric read-scaleogram-from (file-stream-or-name)
  (:documentation "Read the scaleogram contained in the named file or given stream"))

;;;; Methods

(defmethod number-of-scales ((scaleogram-to-analyse scaleogram))
  (.row-count (scaleogram-magnitude scaleogram-to-analyse)))

;;; could be a skeleton or a scaleogram. Perhaps this can be represented by (or scaleogram skeleton)?
(defmethod number-of-octaves (scaleogram-to-analyse)
  (+ (/ (number-of-scales scaleogram-to-analyse) (voices-per-octave scaleogram-to-analyse))
     (skip-highest-octaves scaleogram-to-analyse)))

(defmethod duration-in-samples ((scaleogram-to-analyse scaleogram))
  (.array-dimension (scaleogram-magnitude scaleogram-to-analyse) 1))

(defmethod print-object ((scaleogram-to-print scaleogram) stream)
  (call-next-method scaleogram-to-print stream) ;; to print the superclass.
  (format stream " ~d scales x ~d samples, ~d VPO" 
	  (number-of-scales scaleogram-to-print)
	  (duration-in-samples scaleogram-to-print)
	  (voices-per-octave scaleogram-to-print)))

(defun gaussian-envelope (width &key (mean 0d0) (stddev 1d0) (stddev-range 10.0d0)
			  (scaling (/ 1d0 (* (sqrt (* 2d0 pi)) stddev))))
  ;; to compensate for the sampling rate (width):
  ;; (scaling (/ stddev-range (* (sqrt (* 2d0 pi)) stddev width))))
  "Compute a Gaussian envelope.
   width is the sampling range of an envelope over +/-5 standard deviations.
   mean determines the position of the envelope within the width number of samples.
   stddev determines the standard deviation and therefore the variance of the distribution.
   scaling determines the amplitude peak of the envelope, defaulting to an area of 1.0"
  (let* ((half-stddev-range (/ stddev-range 2.0))
	 (x (.rseq (- half-stddev-range) half-stddev-range width)))
    (.* scaling
	(.exp (.- (./ (.expt (.- x mean) 2.0) (.* 2d0 stddev stddev)))))))

;; (plot (gaussian-envelope 150 :stddev 2d0) (.rseq -5.d0 +5.0 150))
;; (plot (gaussian-envelope 150 :mean -1d0 :stddev 2.5) (.rseq -5.d0 +5.0 150))
;; (plot (gaussian-envelope 150) (.rseq -5.d0 +5.0 150))
;; (plot (gaussian-envelope 144 :scaling 1d0) (.rseq -5.d0 +5.0 144))

(defun skewed-gaussian-envelope (width &key (mean 0d0) (stddev 1d0) 
				 (scaling (/ 1d0 (* (sqrt (* 2d0 pi)) stddev))))
  "Compute a Gaussian envelope skewed so that the scales higher than the mean are half as
   likely as scales lower than the mean." 
  (let* ((skew 2.0)			; factor to skew the higher frequencies by.
	 (x (.rseq (- -5.0d0 mean) (- 5.0d0 mean) width))
	 (skewed-x (.* x (.+ (.* (.< x 0) skew) (.>= x 0)))))
    (.* scaling
 	(.exp (.- (./ (.expt skewed-x 2.0) (.* 2d0 stddev stddev)))))))

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
	 ;;
	 ;; From Grossmann, Kronland-Martinet and Morlet, we multiply the wavelet-scale
	 ;; by the discretised omega value when scaling in the Fourier domain.
	 (omega (.* xi wavelet-scale))) ; (plot omega)
    ;;
    ;; See mallat:tour p76
    ;; According to Holschneider sqrt(2 .* pi) .* 
    ;; check with Daubechies pi ^ (-1/4)
    (.- (.exp (.- (./ (.expt (.- omega omega0) 2) 2)))
	(.exp (.- (./ (.+ (.expt omega 2) (.expt omega0 2)) 2))))))

;;; Plot the gaussian envelope in the Fourier domain
;;; (plot (.realpart (morlet-wavelet-fourier 1024 8)) nil)
;;; (plot (list (.realpart (morlet-wavelet-fourier 1024 16 :omega0 6.2)) 
;;; 	     (.realpart (morlet-wavelet-fourier 1024 16 :omega0 6.45))) nil)
;;; (plot (.realpart (morlet-wavelet-fourier 1024 128)) nil)

(defun plot-time-domain-kernel (signal-time-period wavelet-scale &key (omega0 6.2))
 (let* ((time-wavelet (ifft (morlet-wavelet-fourier signal-time-period wavelet-scale :omega0 omega0)))
	(zero-frequency (/ (.length time-wavelet) 2))
	;; Swap for visualisation around Nyquist frequency
	(swapped-time-wavelet (.concatenate 
			       (.subseq time-wavelet (1+ zero-frequency) (1- signal-time-period))
			       (.subseq time-wavelet 0 zero-frequency))))
   (plot (list (.realpart swapped-time-wavelet) (.imagpart swapped-time-wavelet)) 
	 nil ; TODO (.rseq ? ? signal-time-period)
	 :legends (list "Real Part" "Imaginary Part")
	 :title (format nil "Time domain kernel for omega0 = ~a" omega0))))

;;; (plot-time-domain-kernel 1024 128 :omega0 6.45)
;;; (plot-time-domain-kernel 1024 128 :omega0 6.2)
;;; (plot-time-domain-kernel 1024 128 :omega0 6)
;;; (plot-time-domain-kernel 1024 128 :omega0 5.3364)
;;; (plot-time-domain-kernel 1024 128 :omega0 5)
;;; (plot-time-domain-kernel 1024 128 :omega0 4)
;;; (plot-time-domain-kernel 1024 8)
;;; (plot-time-domain-kernel 1024 16)
;;; (plot-time-domain-kernel 1024 32)

(defun scale-from-period (time-periods voices-per-octave &key (skip-initial-octaves 2))
  "Function to return a vector of scale numbers from periods of time support (in samples).
   The highest two octaves (time domain extent 0-1,1-2) don't tell us much, so we save
   computation time by skipping them."
  (.* voices-per-octave (.- (.log time-periods 2) skip-initial-octaves)))

(defun time-support (scales wavelets-per-octave &key (skip-initial-octaves 2))
  "Function to return a vector of periods of time support (in samples) from scale numbers.
   The highest two octaves (time domain extent 0-1,1-2) don't tell us much, so we save
   computation time by skipping them."
  (.expt 2d0 (.+ (./ scales (.* wavelets-per-octave 1d0)) skip-initial-octaves)))

(defun time-support-seconds (scales wavelets-per-octave sample-rate)
  "Given a sampling rate in Hz, return the IOI in seconds."
  (./ (time-support scales wavelets-per-octave) sample-rate))

(defun uncertainty-of-scale-range (high-scale low-scale voices-per-octave)
  "Return the time range of uncertainty between the two given scales"
  (list (time-support high-scale voices-per-octave) 
	(time-support low-scale voices-per-octave)))

(defun uncertainty-of-period (time-periods voices-per-octave)
  "Return the time range of uncertainty between the two scales spanning a given time period"
  (let* ((exact-scales (scale-from-period time-periods voices-per-octave))
	 (lowest-scales (.floor exact-scales))
	 (highest-scales (.ceiling exact-scales)))
    (list (.- (time-support highest-scales voices-per-octave) time-periods)
	  (.- (time-support lowest-scales voices-per-octave) time-periods))))

;; (.+ 360 (make-narray (uncertainty-of-period 360 16)))   

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
	 ;; (voice-scaling period))
	 
    (format t "Maximum time period analysed = ~d samples, dyadic length = ~d samples~%" 
	    maximum-time-period time-in-samples)

    ;; loop over each voice, beginning with the highest frequency scale.
    (loop 
       for scale-index from 0 below number-of-scales
       do 
	 (let* ((scaled-wavelet (funcall fourier-domain-wavelet time-in-samples (.aref period scale-index) :omega0 6.45d0))
		;; Construct the scaled wavelet in the frequency domain. It will be the same length
		;; as the input-data, but mostly zero outside the Gaussian shape.
		;; Multiply in the Fourier domain and take the inverse FFT, thereby producing the convolution.
		(voice-response (ifft (.* input-data-FFT scaled-wavelet)))
		;; Produce Magnitude/Phase components from Real/Imaginary values.
		
		;; TODO this is currently incorrectly scaled.
		(magnitude-at-scale (.* (.abs voice-response) (.aref voice-scaling scale-index)))
		;; magnitude-at-scale = abs((voice-response .^ 2) ./ (.aref period scale-index)); 
		;; (magnitude-at-scale (.abs voice-response))

		(scale-row (list scale-index t)))

	   (setf (.subarray magnitude scale-row) magnitude-at-scale)
	   ;; If the magnitude value is low, the phase will be ill-conditioned,
	   ;; which we will plot different to significant phase behaviour.
	   ;; atan2 used by arg will return -pi to pi.
	   (setf (.subarray phase scale-row) (.phase voice-response))))
	   ;; (format t "scale-index ~d~%" scale-index)
    (format t "Finished CWT, ~d scales, last time period = ~d samples~%" number-of-scales
	   (floor (.aref period (1- number-of-scales))))
    (values magnitude phase)))

;; TODO test with:
;; (dyadic-cwt :fourier-domain-wavelet #'sombrero-wavelet-fourier)

(defun causal-pad (the-signal padded-length)
  "Pad the start of the-signal with silence. Handles matrices as well
   as signal vectors, in the former case, assuming the columns
   are to be padded to the given length. Returns multiple values
   of the padded signal, and the region to trim (on time-axis)."
  (let* ((signal-rows (.row-count the-signal))
	 (matrix-or-vector (if (equalp signal-rows 1) 0 t)) 
	 (signal-length (.length the-signal))
	 (to-pad (- padded-length signal-length)))
    (if (zerop to-pad)
	(values the-signal (list matrix-or-vector (list 0 (1- signal-length)))) ; Redundant case
	(values
	 ;; Create the zero padded signal.
	 (let* ((start-region (if (eq matrix-or-vector t) (list signal-rows to-pad) to-pad)))
	   (.concatenate (make-double-array start-region) the-signal))
	   ;; Create the .subarray trim description.
	   (list matrix-or-vector (list to-pad (1- padded-length)))))))

(defun end-pad (the-signal padded-length &key (value 0.0d0))
  "Pad the end of the-signal with value, default to silence. Handles matrices as well
   as signal vectors, in the former case, assuming the columns
   are to be padded to the given length. Returns multiple values
   of the padded signal, and the region to trim (on time-axis)."
  (let* ((signal-rows (.row-count the-signal))
	 (matrix-or-vector (if (equalp signal-rows 1) 0 t)) 
	 (signal-length (.length the-signal))
	 (to-pad (- padded-length signal-length)))
    (if (zerop to-pad)
	(values the-signal (list matrix-or-vector (list 0 (1- signal-length)))) ; Redundant case
	(values
	 ;; Create the zero padded signal.
	 (let* ((end-region (if (eq matrix-or-vector t) (list signal-rows to-pad) to-pad)))
	   (.concatenate the-signal (make-double-array end-region :initial-element value)))
	   ;; Create the .subarray trim description.
	   (list matrix-or-vector (list 0 (1- signal-length)))))))

(defun centered-silence-pad (the-signal padded-length)
  "Pad either side of the the-signal with silence to a given length.
   Handles matrices as well as signal vectors, in the former
   case, assuming the columns are to be padded to the given
   length. Returns multiple values of the padded signal, and the
   region to trim (on time-axis)."
  (let* ((signal-rows (.row-count the-signal))
	 (matrix-or-vector (if (equalp signal-rows 1) 0 t)) 
	 (signal-length (.length the-signal))
	 (to-pad (- padded-length signal-length)))
    (multiple-value-bind (half-pad off-by-one) (floor to-pad 2)
      (if (zerop to-pad)
	  ;; Redundant case
	  (values the-signal (list matrix-or-vector (list 0 (1- signal-length))))
	  (values
	   ;; Create the zero padded signal.
	   (let* ((first-region (if (eq matrix-or-vector t) (list signal-rows half-pad) half-pad))
		  (last-region (if (eq matrix-or-vector t)
				   (list signal-rows (+ half-pad off-by-one))
				   (+ half-pad off-by-one))))
	     (.concatenate (make-double-array first-region) the-signal (make-double-array last-region)))
	   ;; Create the .subarray trim description.
	   (list matrix-or-vector (list (+ half-pad off-by-one) (- padded-length half-pad 1))))))))

(defun mirror-pad (the-signal padded-length)
  "Pad either side of the the-signal with mirrored portions of
   the the-signal to a given length. Handles matrices as well as
   signal vectors, in the former case, assuming the columns are
   to be padded to the given length. Returns multiple values of
   the padded signal, trim-dyadic (on time-axis)."
  (let* ((signal-rows (.row-count the-signal))
	 (matrix-or-vector (if (equalp signal-rows 1) 0 t)) 
	 (signal-length (.length the-signal))
	 (to-pad (- padded-length signal-length)))
    (multiple-value-bind (half-pad off-by-one) (floor to-pad 2)
      (if (zerop to-pad)
	  ;; Redundant case
	  (values the-signal (list matrix-or-vector (list 0 (1- signal-length))))
	  (values
	   ;; Padding with the the-signal ensures a periodicity of the window.
	   (let* ((last-region (list (- signal-length (+ half-pad off-by-one)) (1- signal-length)))
		  (lastbit (.subarray the-signal (list matrix-or-vector last-region)))
		  (firstbit (.subarray the-signal (list matrix-or-vector (list 0 (1- half-pad))))))
	     (.concatenate lastbit the-signal firstbit))
	   ;; Create the .subarray trim description.
	   (list matrix-or-vector (list (+ half-pad off-by-one) (- padded-length half-pad 1))))))))

;; (mirror-pad (.rseq 1 10 10) 20)
;; (mirror-pad (.rseq 1 6 6) 8)
;; (mirror-pad (.rseq 1 7 7) 8)
;; (mirror-pad (.rseq 0 509 510) 512)
;; (mirror-pad (.rseq2 1 6 6) 8) ;; TODO needs to manage sequences promoted to column arrays.

(defun dyadic-length (signal-length)
  "Returns the length of the signal if padded to a dyadic (power of two) length."
  (expt 2 (ceiling (log signal-length 2))))

(defun dyadic-pad (signal &key (padding #'mirror-pad))
  "The latest in signal processing hygene, the soft, comfortable dyadic-pad!
   Protects signals from unsightly window edge conditions! ...sorry couldn't resist :^)

   Instead we actually pad either side of the signal with mirrored portions
   of the signal to a dyadic length to enable efficient FFTs.
   Handles matrices as well as signal vectors, in the former case, assuming the columns
   are to be padded to a dyadic length.

   Returns multiple values mirror-pad, trim-dyadic (on time-axis)."
  (funcall padding signal (dyadic-length (.length signal))))

;; (dyadic-pad (.rseq 0 9 10))
;; (dyadic-pad (.rseq2 1 10 10) :padding #'centered-silence-pad)
;; (dyadic-pad (.rseq 1 10 10) :padding #'centered-silence-pad)

(defun number-of-scales-for-period (period &key (voices-per-octave 16))
  "Returns the number of scales that are required to represent the given signal length"
  (floor (scale-from-period (/ (dyadic-length period) 4) voices-per-octave)))

;;; The public interface to the continuous wavelet transform for non-dyadic signals
(defun cwt (time-signal voices-per-octave &key
	    (max-wavelet-period (dyadic-length (./ (.array-dimension time-signal 0) 4)))
	    (padding #'mirror-pad))
  "Pads the signal to a dyadic length, then performs the continuous wavelet transform,
   using dyadic-cwt, trimming off the returned result to match the original signal length.
   Returns a scaleogram instance containing magnitude and phase."
  ;; The wavelet transform operates on 1 x N vector
  (format t "CWT input signal length ~d samples~%" (.array-dimension time-signal 0))
  (multiple-value-bind (padded-signal time-trim) (dyadic-pad time-signal :padding padding)
    (multiple-value-bind (padded-magnitude padded-phase)
	(dyadic-cwt padded-signal voices-per-octave max-wavelet-period)
      (let* ((scale-rows (.array-dimension padded-magnitude 0))
	     (trim (list (list 0 (1- scale-rows)) (second time-trim)))
	     (scaleogram-result (make-instance 'scaleogram
					       :time-trim trim
					       :magnitude (.subarray padded-magnitude trim)
					       :phase (.subarray padded-phase trim)
					       :dyadic-padded-magnitude padded-magnitude
					       :dyadic-padded-phase padded-phase
					       :voices-per-octave voices-per-octave)))
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

;;; Just sum over all scales, across time, either the magnitude or correlated ridge peaks,
;;; or other time-frequency planes.
;;; TODO This is pretty slow, due to the n^2 .partial-sum function and the .transpose.
(defun scale-persistency (scale-peaks)
  "Returns the normalised measure of contribution of each scale in the time-frequency plane"
  (if scale-peaks
      (./ (.partial-sum (.transpose scale-peaks) :dimension 1)
	  (.array-dimension scale-peaks 1))
      nil))

(defmethod phase-of ((ridges list) (rhythm-scaleogram scaleogram))
  "Returns the phase of the list of ridges, by inversion of the scaleogram"
  (let* ((time-frequency-dimensions (.array-dimensions (scaleogram-magnitude rhythm-scaleogram)))
	 ;; The right way to do this is with a tight (well, two octave) Gaussian envelope over it.
	 ;; (ridges-mag (gaussian-envelope ridges voices-per-octave))
	 ;; But just a single voice alone produces the correct oscillation, with lower amplitude:
	 (empty-magnitude (make-double-array time-frequency-dimensions))
	 (ridges-mag (dolist (ridge ridges empty-magnitude)
			     (insert-ridge ridge empty-magnitude :constant-value 1d0))))
    ;; TODO To be completely correct the original padded output from dyadic-cwt should be used
    ;; for input to the dyadic-icwt.
    ;; TODO create a scalogram
    ;; (clamped-magnitude-scaleogram (copy-instance rhythm-scaleogram))
    ;; (setf (scaleogram-magnitude clamped-magnitude-scalogram) ridges-mag)
    ;; (icwt clamped-magnitude-scaleogram)
    ;; Use the modified magnitude and original phase to compute a sinusoid and it's
    ;; Hilbert transform, from that, determine the phase of the sinusoid.
    (.phase (icwt ridges-mag (scaleogram-phase rhythm-scaleogram) (voices-per-octave rhythm-scaleogram)))))

(defmethod phase-of ((a-ridge ridge) (rhythm-scaleogram scaleogram))
  "Returns the phase of a single ridge, by inversion of the scaleogram"
  (phase-of (list a-ridge) rhythm-scaleogram))

;;; File I/O

;;; For now just write these as binary files. Eventually they should be replaced with
;;; writing n-double-arrays, ints and strings using the NLISP HDF5 I/O routines
;;; (still needing to be written).
(defmethod save-to-file ((scaleogram-to-write scaleogram) (file-stream stream))
  (nlisp::write-u4-littleendian file-stream (voices-per-octave scaleogram-to-write))
  (nlisp::write-u4-littleendian file-stream (skip-highest-octaves scaleogram-to-write))
  (nlisp::save-to-binary-stream (scaleogram-magnitude scaleogram-to-write) file-stream)
  (nlisp::save-to-binary-stream (scaleogram-phase scaleogram-to-write) file-stream))

;; TODO (.save (list (voices-per-octave scaleogram-to-write) (skip-highest-octaves scaleogram-to-write) (scaleogram-magnitude scaleogram-to-write) (scaleogram-phase scaleogram-to-write)))

(defmethod save-to-file ((scaleogram-to-write scaleogram) (filename pathname))
  "Writes the scaleogram to the named file"
  (with-open-file (file-stream filename :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (save-to-file scaleogram-to-write file-stream)))

(defmethod read-scaleogram-from ((file-stream stream))
  "Reads and returns a new scaleogram instance, returns nil when EOF"
  (let* ((voices-per-octave (nlisp::read-u4-littleendian file-stream))
	 (skip-highest-octaves (nlisp::read-u4-littleendian file-stream))
	 (magnitude (nlisp::read-binary-stream file-stream))
	 (phase (nlisp::read-binary-stream file-stream)))
    (make-instance 'scaleogram 
		   :voices-per-octave voices-per-octave
		   :skip-highest-octaves skip-highest-octaves
		   :magnitude magnitude
		   :phase phase)))

(defmethod read-scaleogram-from ((filename pathname))
  "Read the scaleogram contained in the named file"
  (with-open-file (file-stream filename :direction :input :element-type '(unsigned-byte 8))
    (read-scaleogram-from file-stream)))

;; TODO in principle, this routine still has value and just needs some judicious read-u4-littleendians.
;; (defmethod read-scaleogram-dimensions-from ((filename pathname))
;;   "Reads just the dimensions of the scaleogram contained in the named file. 
;; This is *much* quicker than reading the two matrices."
;;     )

