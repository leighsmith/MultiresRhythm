## -*- Octave -*-
##
## $Id$
## Compute points of stationary phase
##  Usage
##    ridges = ridgesStatPhase(phase,par)
##  Inputs
##    mag    Magnitude output of CWT
##    phase  Phase output of CWT
##  Outputs
##    sp - binary array indicating presence of ridge.
##
##  Description
##    Implementation of Delprat et. al's ridges from points of stationary phase
##    convergent algorithm. See: delprat:asymptotic
##
##    By Leigh Smith 3/9/1998
##
##  the scale is the central frequency of the motherwavelet divided by
## the central frequency of the dilated wavelet.

function statPhase = stationaryPhase(mag, phase, voicesPerOctave)
  magnitude_minimum = 0.01;	# minimum magnitude to accept phase as valid

  sz = size(phase);
  nScale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);
  signalPhaseDiff = zeros(sz);

  ## Compute a discrete approximation to the partial derivative of the
  ## phase with respect to translation (time) t.
  dt_phase = diff(phase.').';

  ## Phase is -pi -> pi.
  ## We need to add back 2 pi to compensate for the phase wraparound
  ## from pi to -pi. This wraparound will create a dt_phase close to 2 pi.
  whichwrap = abs(dt_phase) > (1.5 * pi); # anything > pi is a wraparound
  phasewrap = 2 * pi - abs(dt_phase);
  ## wrapped_dt_phase = (whichwrap == 0) .* dt_phase + phasewrap .* whichwrap;
  wrapped_dt_phase = (!whichwrap) .* dt_phase + phasewrap .* whichwrap;

  ## The acceleration of phase difference should be non-zero
  ## (Tchamitchian and Torresani Eq. III-7 pp128)
  ## phaseDiffAcceleration will therefore be two time sample points less
  ## than the original phase time extent.
  ## phaseDiffAcceleration = diff(wrapped_dt_phase.').';

  ## Doing this will still produce NaN since the division already
  ## creates Inf and the clamping uses multiplication.
  ## wrapped_dt_phase = clampToBounds(wrapped_dt_phase, mag, magnitude_minimum, 1);

  ## convert the phase derivative into periods in time
  signalPeriod = (2 .* pi) ./ wrapped_dt_phase;


  ## When we have neglible magnitude, phase is meaningless, but can
  ## oscillate so rapidly we have two adjacent phase values which are
  ## equal (typically pi, pi/2, or 0). This can cause dt_phase to be
  ## zero and therefore make signalPeriod NaN. This then causes
  ## normaliseByScale to freakout because NaN is considered a maximum.
  ## Our kludge is to set those NaN signalPeriods to 0.0 so they don't
  ## cause excessive extrema and they will be zeroed out of the final
  ## result using clampToBounds(). Strictly speaking, we should verify
  ## mag is below magnitude_minimum before setting these zero dt_phase values.
  ## Use fortran indexing.
  signalPeriod(find(!dt_phase)) = 0.0;

  scaleTimeSupport = timeSupport(1:nScale,voicesPerOctave);

  for i=1:nScale
    ## Accept if the signal period (from its phase derivatives) and
    ## the time support of the wavelet at each dilation scale are within
    ## 1.5 sample of one another.
    signalPhaseDiff(i,1:ntime-1) = abs(signalPeriod(i,:) - scaleTimeSupport(i));
  endfor

  normSignalPhaseDiff = normaliseByScale(signalPhaseDiff);

  ## We invert the normalised phase difference so that maximum values
  ## indicate stationary phase -> ridges.
  maximalStatPhase = ones(sz) - normSignalPhaseDiff;

  ## There must be some magnitude at the half-plane points of stationary phase
  ## for the ridge to be valid. 
  statPhase = clampToBounds(maximalStatPhase, mag, magnitude_minimum, 0);

endfunction



