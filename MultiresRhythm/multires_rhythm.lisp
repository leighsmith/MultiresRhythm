;;;; $Id:$

;;; (in-package multires-rhythm)

(defclass rhythm
description
signal
sample-rate
)

  ;; We set any ill-conditioned phase (negligible magnitude) to zero here
  ;; after we take the phase derivative (in ridgesStatPhase) as it would
  ;; create false changes.
  phase = cleanPhase(mag, phase);

(defun preferred-tempo (magnitude phase voices-per-octave sample-rate)
  )

#|
  (format t "Stationary Phase~%")
  (format t "Local Phase Congruency~%")
  ;; redundant
  ;;fprintf(promptStream, "Stationary Phase Ridges\n");
  ;;ridgeStatPhase = ridgesStatPhase(magnitude, phase, voices-per-octave);
  fprintf(promptStream, "Finding ridge (by scale)\n");

  ;; show what we got as an intensity plot
  plotCWT("correlation", correlation)
  )
|#

(defun stationary-phase (magnitude phase voices-per-octave)
)

(defun local-phase-congruency (magnitude phase)
)

(defun normalise-by-scale (magnitude)
"Normalise an analysis finding the maximum scale at each time point.
 We assume values are positive values only"
  sz = (.array-dimensions size(analysis);
  nScale = sz(1);
  nTime = sz(2);

  maxVals = zeros(sz);
  ;; Determine the maximum scale at each time.
  maxScalePerTime = max(abs(analysis));
  ;; If a maximum scale value is 0, then make those 1 to keep the
  ;; division healthy.
  maxScalePerTime = maxScalePerTime + !maxScalePerTime;
  ;; Duplicate the maximum scales per time for each scale to enable
  ;; element-wise division.
  for i = 1:nScale
    maxVals(i,:) = maxScalePerTime;
  endfor
  normalised = analysis ./ maxVals;
)

;; otherwise return normalisedMagnitude, statPhase or localPC individually.
(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  ;; ahh, parallel machine anyone??
  (let ((stat-phase (stationary-phase magnitude phase voices-per-octave))
	(local-pc (local-phase-congruency magnitude phase))
	;; magnitude = abs(magnitude)
	(normalised-magnitude (normalise-by-scale magnitude)))
    ;; correlate (by averaging) the energy modulus, stationary phase and
    ;; local phase congruency. Since statPhase and local PC are both
    ;; conditioned on a minimal magnitude, we reduce false positives.
    (./ (.+ normalised-magnitude stat-phase local-pc) 3d0)))

(defun extract-ridges (scale-peaks)
  (extract-ridge-walking-hills ))

;; TODO Or should the tempo preferencing influence the selection?
(defun select-longest-tactus (ridge-set)
)

;; TODO could make this a method for rhythm
(defun tactus-for-rhythm (rhythm &key (voices-per-octave 16))
  "Returns the selected tactus given the rhythm."
  (multiple-value-bind (mag phase) (cwt (rhythm signal) voices-per-octave)
    ;; Correlate various ridges to produce a robust version.
    (let* ((correlated-ridges (correlate-ridges magnitude phase voices-per-octave))
	   ;; Scale index 1 is the highest frequency (smallest dilation) scale.
	   (salient-scale (preferred-tempo mag phase voices-per-octave (rhythm sample-rate)))
	   ;; Weight by the absolute tempo preference.
	   (tempo-weighted-ridges (.* correlated-ridges (tempo-salience salient-Scale)))
	   (ridge-set (extract-ridges tempo-weighted-ridges)))
      ;; select out the tactus from all ridge candidates.
      (select-longest-tactus ridge-set))))
