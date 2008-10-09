## -*- Octave -*-
## we clamp any ill-conditioned phase (negligble magnitude) to the value 
## given here (defaulting to zero).
function newphase = cleanPhase(magnitude, phase, threshold, clamp)
  if(nargin < 4)
    clamp = 0;
  endif
  if(nargin < 3)
    threshold = 0.001;
  endif
  thresholdMag = magnitude > threshold;
  newphase = (thresholdMag .* phase) + (!thresholdMag .* clamp);
  ##newphase = clampToBounds(phase,magnitude,threshold,clamp);
endfunction
