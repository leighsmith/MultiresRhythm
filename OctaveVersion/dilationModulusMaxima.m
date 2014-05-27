## -*- Octave -*-
##
## $Id$
##
## Compute the peaks in the dilation energy profile.
##  Usage
##    maxmap = ridges(magnitude,par)
##  Inputs
##    magnitude    Magnitude output of CWT
##    par    optional. If present, keep thresholds only
##           above a certain value. UNUSED AT MOMENT
##  Outputs
##    maxmap binary array indicating presence of max or not
##
##  Description
##    Used to calculate fractal exponents etc. 
## Modified from the Wavelab version.
##
function maxmap = dilationModulusMaxima(magnitude,par)
  magnitude_minimum = 0.01;

  sz = size(magnitude);
  nscale = sz(1);
  ntime = sz(2);
  
  maxmap = zeros(sz);

  d      = 1:nscale;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);
  ## magnitude    = abs(magnitude);
    
  ## identify energy peaks across the dilation scale axis at each time point.
  for t=1:ntime,
    localmax = magnitude(d,t) > magnitude(dplus,t) & \
	magnitude(d,t) > magnitude(dminus,t) & \
	magnitude(d,t) > magnitude_minimum;
    ## this retains the modulus peak values, clamping non maxima to zero.
    maxmap(:,t) = localmax .* magnitude(:,t);
    ## maxmap(:,t) = localmax;
  endfor
endfunction  


