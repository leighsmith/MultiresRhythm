## -*- Octave -*-
##
## $Id$
## Compute ridges from points of stationary phase
##  Usage
##    ridges = ridgesStatPhase(mag, phase, voicesPerOctave)
##  Inputs
##    mag    Magnitude output of CWT
##    phase  Phase output of CWT
##    voicesPerOctave
##  Outputs
##    ridges - binary array indicating presence of ridge.
##
##  Description
##    Implementation of Delprat et. al's ridges from points of stationary phase
##    convergent algorithm. See: delprat:asymptotic and tchamitchian:ridge.
##
##    By Leigh Smith 3/9/1998
##
## The scale is the central frequency of the mother-wavelet divided by
## The central frequency of the dilated wavelet.

function ridges = ridgesStatPhase(mag,phase,voicesPerOctave)
  ## the minimum magnitude to consider we still have a legitimate signal
  ## worth considering it's phase
  magnitude_minimum = 0.01;
  ## The number of samples difference between the time support of the
  ## dilated wavelet and the signal period.
  ## 1.5 was used for impulsive signals, but 0.5 functions fine for
  ## classic examples such as Tchamitchian and Torresani's.
  tolerance = 0.5; 

  sz = size(phase);
  nScale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);
  signalPhaseDiff = zeros(sz);

  ## Compute a discrete approximation to the partial derivative of the
  ## phase with respect to translation (time) t.
  dt_phase = diff(phase.').';

  ## Phase is -pi -> pi.
  ## We need to add back 2 pi to compensate for the phase wrap around.
  ## wrapping will create a dt_phase close to 2 pi.
  whichwrap = abs(dt_phase) > (1.5 * pi); # anything > pi is a wraparound
  phasewrap = whichwrap .* (2 * pi - abs(dt_phase));
  wrapped_dt_phase = (whichwrap == 0) .* dt_phase + phasewrap .* whichwrap;

  ## convert the phase derivative into periods in time
  signalPeriod = (2 .* pi) ./ wrapped_dt_phase;

  scaleTimeSupport = timeSupport(1:nScale,voicesPerOctave);

  for i=1:nScale
    ## Accept if the signal period (from its phase derivatives) and
    ## the time support of the wavelet at each dilation scale are
    ## within tolerance portion of samples of one another.
    signalPhaseDiff(i,1:ntime-1) = \
	abs(signalPeriod(i,:) - scaleTimeSupport(i)) <= tolerance;
  endfor

  ## There must be some magnitude at the half-plane points of stationary phase
  ## for the ridge to be valid.
  ridges = clampToBounds(signalPhaseDiff, mag, magnitude_minimum, 0);

endfunction
