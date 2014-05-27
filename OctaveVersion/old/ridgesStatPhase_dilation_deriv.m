## -*- Octave -*-
## ridgesStatPhase -- Compute ridges from points of stationary phase
##  Usage
##    ridges = ridgesStatPhase(phase,par)
##  Inputs
##    phase  Phase output of CWT
##    par    optional. If present, keep thresholds only
##           above a certain value. UNUSED AT MOMENT
##  Outputs
##    ridges - binary array indicating presence of ridge.
##
##  Description
##    Implementation of Delprat et. al's ridges from points of stationary phase
##    convergent algorithm. See: delprat:asymptotic
##
##    By Leigh Smith 3/9/1998
##
##  the scale is the central frequency of the motherwavelet divided by
## the central frequency of the dilated wavelet.

function ridges = ridgesStatPhase(mag,phase,voicesPerOctave)
  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz(1),sz(2)-1);
  signalPhaseDiff = zeros(sz(1),sz(2)-1);

  ## Compute a discrete approximation to the partial derivative of the
  ## phase with respect to translation (time) t.
  dt_phase = diff(phase.').';

  ## Phase is -pi -> pi.
  ## We need to add back 2 pi to compensate for the phase wrap around.
  ## wrapping will create a dt_phase close to 2 pi.
  whichwrap = abs(dt_phase) > (1.5 * pi);
  phasewrap = whichwrap .* (2 * pi - abs(dt_phase));
  wrapped_dt_phase = (whichwrap == 0) .* dt_phase + phasewrap .* whichwrap;

  ## Any remaining negative values are from the edges of zeroed phase
  ## regions due to negligble magnitude...we nuke 'em.
  ## it does mean we clip some subtle negative values,
  ##  so we keep those > -1.
  ## wrapped_dt_phase = (wrapped_dt_phase > -1) .* wrapped_dt_phase;

  ## Need to ensure the derivative of the phase reveals the frequency
  ## at the given scale. How?

  ## check the derivative of the phase is not zero...
  ## divisiblePhaseDeriv = wrapped_dt_phase + (abs(wrapped_dt_phase) < 1.0e-9) * 1.0e+9;
  ## derivative of the phase should be producing omega1 defined in 6.5
  ##r_scale = nscale - (dilation_deriv ./ divisiblePhaseDeriv);
  ##ridges = divisiblePhaseDeriv
  dilation_deriv = (2 .* pi) ./ timeSupport(1:nscale,voicesPerOctave);
  ##for i=1:nscale
  ##  signalPhaseDiff(i,:) = abs(wrapped_dt_phase(i,:) - dilation_deriv(i));
  ##endfor
dil_sub = dilation_deriv(62:66)
wrap_sub = wrapped_dt_phase(62:66,3050:3325)
save "dilwrap" dil_sub wrap_sub

  for i=1:nscale
    ## accept if the signal and wavelet phase derivatives are within
    ## 1.5% of one another.
    signalPhaseDiff(i,:) = (abs(wrapped_dt_phase(i,:) - dilation_deriv(i)) ./ \
			    dilation_deriv(i)) < 0.009;
    ## unclampedRidges(i,:) = signalPhaseDiff(i,:);
  endfor

  ridges = clampToBounds(signalPhaseDiff,mag(:,1:ntime-1),0.01,0);
  ## ridges = signalPhaseDiff;

  ##signalPhaseDiff(:,1);
  ## lowestDiffs = min(signalPhaseDiff)
  ## for i=1:nscale
  ##  ridges(i,:) = signalPhaseDiff(i,:) == lowestDiffs;
  ##endfor

endfunction  

