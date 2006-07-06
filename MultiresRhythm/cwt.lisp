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

;; voicesPerOctave
(defun gaussian-envelope (width)
  "Compute a gaussian envelope.
   width is in the number of points to range an envelope over 3 standard deviations."
  (let ((x (.rseq -0.99 1.00 width)))
    (.exp (.- (.expt (.* 2d0 x (/ 6d0 (sqrt 8d0))) 2.0)))))

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

Uses the morlet-wavelet-fourier function to give us a scaled version of the wavelet.
Length of the input-data is assumed to be a power of 2 (dyadic).
Does the convolution by multiplying in the Fourier domain.
The maximum-time-period can really only be about 1/4 the input-data to
still be representing the wavelets in the frequency and time domains meaningfully.
"
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
		;; magnitude-at-scale = (./ (.abs voice-response) (.aref voice-scaling scale-index))
		;; magnitude-at-scale = abs((voice-response .^ 2) ./ (.aref period scale-index)); 
		(magnitude-at-scale (.abs voice-response))

		(scale-row (list scale-index t)))

	   (setf-subarray (val magnitude) (val magnitude-at-scale) scale-row)
	   ;; If the magnitude value is low, the phase will be ill-conditioned,
	   ;; which we will plot different to significant phase behaviour.
	   ;; atan2 used by arg will return -pi to pi.
	   (setf-subarray (val phase) (val (.phase voice-response)) scale-row)
	   (format t "scale-index ~d~%" scale-index)))
    (format t "Finished CWT, last time period = ~d~%" (.aref period (1- number-of-scales)))
    (values magnitude phase)))

;; TODO test with:
;; (dyadic-cwt :fourier-domain-wavelet #'sombrero-wavelet-fourier)

(defun dyadic-length (signal-length)
  "Returns the length of the signal if padded to a dyadic (power of two) length."
  (expt 2 (ceiling (log signal-length 2))))

(defun dyadic-pad (signal)
  "The latest in signal processing hygene, the soft, comfortable dyadic-pad!
Protects signals from unsightly window edge conditions! ...sorry couldn't resist :-)

Instead we actually pad either side of the signal with mirrored portions
of the signal to a dyadic length to enable efficient FFTs.

Returns multiple values pad-signal, trim-dyadic (on time-axis).
"
  (let* ((signal-length (.array-dimension signal 0))
	 (padded-length (dyadic-length signal-length))
	 (to-pad (- padded-length signal-length)))
    (multiple-value-bind (half-pad off-by-one) (floor to-pad 2)
      ;; we can generate a empty matrix warning if the newLength matches the
      ;; signal-length.
      (values 
       (if (zerop to-pad)
	   ;; Redundant case
	   signal
	   ;; Create the padded signal.
	   ;; padding with the signal ensures a periodicity of the window
	   ;; TODO alternatively pad with zeros:
	   ;; pad-signal = [zeros(half-pad + off-by-one, 1); signal; zeros(half-pad, 1)];
	   (let* ((last-region (list (- signal-length (+ half-pad off-by-one)) (1- signal-length)))
		  (lastbit (.subarray signal (list 0 last-region))))
	     (.concatenate lastbit signal (.subarray signal (list 0 (list 0 (1- half-pad)))))))
       
       ;; Create the .subarray trim description.
       (if (zerop to-pad)
	   (list 0 (1- signal-length))
	   (list (+ half-pad off-by-one) (- padded-length half-pad 1)))))))

;; (dyadic-pad (.rseq 0 9 10))

;;; The public interface to the continuous wavelet transform for non-dyadic signals
(defun cwt (signal voices-per-octave 
	    &key (max-wavelet-period (dyadic-length (./ (.array-dimension signal 0) 4))))
  "Pads the signal to a dyadic length, then performs the continuous wavelet transform,
   using dyadic-cwt, trimming off the returned result to match the original signal length.
   Returns multiple items (magnitude phase)"
  ;; The wavelet transform operates on 1 x N vector
  (multiple-value-bind (pad-signal time-trim) (dyadic-pad signal)
    (multiple-value-bind (padded-magnitude padded-phase)
	(dyadic-cwt pad-signal voices-per-octave max-wavelet-period)
      (let* ((scale-rows (.array-dimension padded-magnitude 0))
	     (trim (list (list 0 (1- scale-rows)) time-trim)))
	(values (.subarray padded-magnitude trim) (.subarray padded-phase trim))))))
