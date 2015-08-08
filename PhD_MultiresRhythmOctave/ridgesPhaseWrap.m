# -*- Octave -*-
#

function ridges = ridgesPhaseWrap(mag,phase,par)
  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);

  ## Compute a discrete approximation to the partial derivative of the
  ## phase with respect to translation (time) t.
  dt_phase = diff(phase.').';

  ridges = zeros(nscale,ntime);
  for i = 1:nscale
    intv = phase_wrap_interval(dt_phase, i);
    match_dilation = round(timeSupport(i,16));
    oscillation = intv == match_dilation;
    oscpos = find(oscillation);
    for j = 1:length(oscpos)
      ## only get the first ridge element
      ridgeTime = sum(intv(1:oscpos(j)));
      ridges(i,ridgeTime) = 1;
    endfor
  endfor
endfunction

