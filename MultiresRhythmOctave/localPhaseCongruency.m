## -*- Octave -*-
##
## $Id$
## Compute points of local phase congruency
##  Usage
##    ridges = localPhaseCongruency(mag,phase)
##  Inputs
##    mag    Magnitude output of CWT - currently ignored.
##    phase  Phase output of CWT
##  Outputs
##    normalised_congruency - matrix indicating presence of congruency.
##
##  Description
##    Determines the points of phase with least deviation between
##    adjacent scales.
##
##    By Leigh Smith <leigh@tomandandy.com>
##

function normalisedCongruency = localPhaseCongruency(mag,phase)
  magnitude_minimum = 0.01;	# minimum magnitude to accept phase as valid
  congruency_threshold = 0.05;

  sz = size(phase);
  nScale = sz(1);
  nTime = sz(2);
  
  ridges = zeros(sz);

  ## find derivative with respect to scale s.
  abs_ds_phase = abs(diff(phase));

  ## Compensate for phase wrap-around with derivatives wrt scale such that
  ## the maximum angular difference between vectors <= pi radians.
  whichwrap = abs_ds_phase > pi;
  phasewrap = 2 .* pi - abs_ds_phase;

  ## Compute the troughs in the phase congruency, which indicates where
  ## phase most closely match between adjacent scales, most indicative
  ## of a frequency. 
  localPhaseDiff = (!whichwrap) .* abs_ds_phase + whichwrap .* phasewrap;

  ## Since we produce the derivative from the difference, localPhaseDiff
  ## is one element less, so we need to interpolate the result to
  ## produce the correct value for each scale.
  scalePad = zeros(1,nTime);
  congruency = abs([localPhaseDiff; scalePad] - [scalePad; localPhaseDiff]) / 2;

  ## determine outliers, those troughs greater in difference than threshold
  noOutliers = congruency < congruency_threshold;

  ## The local phase congruency measure should be maximum for least
  ## difference, so we normalise it, then subtract from 1.
  maximalLocalPC = ones(sz) - normaliseByScale(congruency);

  ## There must be some magnitude at the half-plane points of local
  ## phase congruency for the ridge to be valid.
  ## Remove outliers.
  normalisedCongruency = clampToBounds(maximalLocalPC, mag, \
				       magnitude_minimum, 0) .* noOutliers;

endfunction
