## There is something interesting about the first derivative of the
## phase wrt scale.

function ridges = ridgesScalePhase(phase,par)
  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);

  d_s_phase = diff(phase);
##  ridges(1:nscale-1,:) = abs(d_s_phase) > pi;
  ridges(1:nscale-1,:) = d_s_phase == 0;

endfunction  
