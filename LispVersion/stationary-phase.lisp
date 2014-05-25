(defun stationary-phase (magnitude phase voices-per-octave &key (magnitude-minimum 0.005))
  "Compute points of stationary phase. Implementation of Delprat
   et. al's ridges from points of stationary phase convergent
   algorithm. See: delprat:asymptotic

   Inputs
     magnitude    Magnitude output of CWT
     phase        Phase output of CWT
     magnitude-minimum is the minimum magnitude to accept phase as valid
   Outputs
     binary array indicating presence of ridge."
  ;; the scale is the central frequency of the mother wavelet divided by
  ;; the central frequency of the dilated wavelet.
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (last-time (- time-in-samples 2)) ; phase-diff will reduce the length by 1.
	 (signal-phase-diff (make-double-array time-frequency-dimensions))

	 ;; Compute a discrete approximation to the partial derivative of the
	 ;; phase with respect to translation (time) t.
	 (wrapped-dt-phase (phase-diff phase))

	 ;; The acceleration of phase difference should be non-zero (Tchamitchian and
	 ;; Torresani Eq. III-7 pp128) phase-diff-acceleration will therefore be two time
	 ;; sample points less than the original phase time extent.
	 ;; (phase-diff-acceleration (.diff wrapped-dt-phase))
	 ;;
	 ;; Doing this will still produce NaN since the division already
	 ;; creates Inf and the clamping uses multiplication.
	 ;; wrapped-dt-phase = (clamp-to-bounds wrapped-dt-phase magnitude magnitude-minimum 1)

	 ;; convert the phase derivative into periods in time
	 (signal-period (./ (* 2 pi) wrapped-dt-phase))
	 ;; Calculate the time support of each dilated wavelet.
	 (scale-time-support (time-support (.iseq 0 (1- number-of-scales)) voices-per-octave))
	 (clipped-signal-phase-diff)
	 (maximal-stationary-phase))

    ;; When we have neglible magnitude, phase is meaningless, but can oscillate so rapidly
    ;; we have two adjacent phase values which are equal (typically pi, pi/2, or 0). This
    ;; can cause dt-phase to be zero and therefore make signal-period NaN. This then
    ;; causes normalise-by-scale to freak out because NaN is considered a maximum.  Our
    ;; kludge is to set those NaN signal-periods to 0.0 so they don't cause excessive
    ;; extrema and they will be zeroed out of the final result using
    ;; clamp-to-bounds. Strictly speaking, we should verify mag is below magnitude-minimum
    ;; before setting these zero dt-phase values.
    ;; (setf (.aref signal-period (find (.not dt-phase))) 0.0)

    ;; Should accept if the signal period (from its phase derivatives) and the time support of
    ;; the wavelet at each dilation scale are within 1.5 sample of one another. Since we
    ;; are correlating with other ridge methods, we instead rank if the match is within
    ;; one time-support period.
    (dotimes (scale-index number-of-scales)
      (setf (.subarray signal-phase-diff (list scale-index (list 0 last-time)))
	    (./ (.abs (.- (.row signal-period scale-index) (.aref scale-time-support scale-index)))
		(.aref scale-time-support scale-index))))

    (setf clipped-signal-phase-diff (clamp-to-bounds signal-phase-diff signal-phase-diff
						     :high-bound 1d0 :clamp-high 1d0))

    ;; We invert the phase difference ratio so that maximum values indicate stationary phase -> ridges.
    (setf maximal-stationary-phase (.- 1d0 clipped-signal-phase-diff))

    ;; There must be some magnitude at the half-plane points of stationary phase for the ridge to be valid.
    (clamp-to-bounds maximal-stationary-phase magnitude :low-bound magnitude-minimum :clamp-low 0)))
