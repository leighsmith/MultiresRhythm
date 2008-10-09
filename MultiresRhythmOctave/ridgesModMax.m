## -*- Octave -*-
## ridgesModMax -- compute ridges across the translation axis
##  Usage
##    maxmap = ridgesModMax(cwt,par)
##  Inputs
##    cwt    Magnitude output of CWT
##    par    optional. If present, keep thresholds only
##           above a certain value. UNUSED AT MOMENT
##  Outputs
##    maxmap binary array indicating presence of max or not
##
##  Description
##    Used to calculate fractal exponents etc. 
##    Modified from the Wavelab version by Leigh Smith 30/7/1998.
##
function maxmap = ridgesModMax(magnitude,par)
  sz = size(magnitude);
  nscale = sz(1);
  ntime = sz(2);
  
  maxmap = zeros(sz);

  t      = 1:ntime;
  tplus  = shift(t, 1);
  tminus = shift(t, -1);
  magnitude = abs(magnitude);
    
  ## identify peaks across the translation (time) axis for each dilation (scale).
  ## Actually, to make the Phase Congruency easier to see, we don't do the
  ## highest two octaves as we know they will locate on the beat.
  for d= 1:nscale,
    localmax = magnitude(d,t) > magnitude(d,tplus) & \
	magnitude(d,t) > magnitude(d,tminus) & \
	magnitude(d,t) > 1.0e-4;
    ## this retains the modulus peak values, clamping non maxima to zero.
    ## maxmap(:,t) = localmax .* magnitude(:,t);
    maxmap(d,:) = localmax;
  endfor
endfunction  

