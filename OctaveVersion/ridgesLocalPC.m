## -*- Octave -*-
## ridgesLocalPC -- Compute ridges from points of local phase congruency
##  Usage
##    ridges = ridgesLocalPC(phase,par)
##  Inputs
##    phase  Phase output of CWT
##    par    optional. If present, keep thresholds only
##           above a certain value. UNUSED AT MOMENT
##  Outputs
##    ridges - binary array indicating presence of ridge.
##
##  Description
##    Determines the points of phase with least deviation across scales.
##
##    By Leigh Smith 3/9/1998
##

function ridges = ridgesLocalPC(mag,phase,par)
  precision = 1.0e-4; # difference to consider phase derivatives differ
  magnitude_minimum = 0.01;
  congruency_threshold = 0.05;

  sz = size(phase);
  nScale = sz(1);
  nTime = sz(2);
  
  ridges = zeros(sz);

  d      = 1:nScale-1;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);

  ## find derivative with respect to scale s.
  abs_ds_phase = abs(diff(phase));

  ## Compensate for phase wrap-around with derivatives wrt scale such that
  ## the maximum angular difference between vectors <= pi radians.
  whichwrap = abs_ds_phase > pi;
  phasewrap = 2 .* pi - abs_ds_phase;
  congruency = (!whichwrap) .* abs_ds_phase + whichwrap .* phasewrap;

  ## Identify congruency troughs across the dilation scale axis
  ## at each time point. 

  for t=1:nTime
    localmin = (congruency(dplus,t) - congruency(d,t)) > precision  & \
	(congruency(dminus,t) - congruency(d,t)) > precision & \
	mag(d,t) > magnitude_minimum;
    peak_congruent = congruency(:,t); # wrt 0 as highest congruency
    ## remove outliers, troughs greater in difference than threshold
    noOutliers = peak_congruent < congruency_threshold;
    ## a = localmin & noOutliers .* congruency(d,t);
    a = localmin & noOutliers;
    ridges(1:nScale-1,t) = a;
  endfor

  ##plotRidgesProfile("local phase cong", 492, mag, congruency, ridges);
  ##pause

endfunction  
