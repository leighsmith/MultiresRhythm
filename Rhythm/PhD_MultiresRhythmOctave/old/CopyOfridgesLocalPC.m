## -*- Octave -*-
## ridgesStatPhase -- Compute ridges from points of local phase congruency
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
##  the scale is the central frequency of the motherwavelet divided by
## the central frequency of the dilated wavelet.

function ridges = ridgesLocalPC(mag,phase,par)
  precision = 1.0e-4; # difference to consider phase derivatives differ
  magnitude_minimum = 0.001;
  congruency_threshold = 0.05;

  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);

  d      = 1:nscale-1;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);

  ## With respect to scale s.
  ds_phase = diff(phase);

  ## Compensate for phase wrap-around with derivatives wrt scale such that
  ## the maximum angular difference between vectors <= pi radians.
  whichwrap = abs(ds_phase) > pi;
  phasewrap = whichwrap .* (2 * pi - abs(ds_phase));
  ## wrapped_ds_phase = (whichwrap == 0) .* ds_phase + phasewrap .* whichwrap;
  congruency = (whichwrap == 0) .* ds_phase + phasewrap .* whichwrap;

  ## congruency = abs(wrapped_ds_phase);

  ## d2s_phase = diff(wrapped_ds_phase);

  ## Identify congruency troughs across the dilation scale axis
  ## at each time point. 

  for t=1:ntime,
    localmin = (congruency(dplus,t) - congruency(d,t)) > precision  & \
	(congruency(dminus,t) - congruency(d,t)) > precision & \
	mag(d,t) > magnitude_minimum;
    ## this retains the trough values, clamping non-minima to zero.
    ## peak_congruent = localmin .* congruency(:,t);
    peak_congruent = congruency(:,t); # wrt 0
    ## remove outliers
    ## noOutliers = abs(peak_congruent - mean(peak_congruent)) <= std(peak_congruent);
    noOutliers = peak_congruent < congruency_threshold;
    ridges(1:nscale-1,t) = localmin & noOutliers;
  endfor
  
endfunction  
