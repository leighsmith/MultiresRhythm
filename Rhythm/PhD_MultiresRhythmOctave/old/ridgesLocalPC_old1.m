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
  precision = 1.0e-4;
  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);

  d      = 1:nscale-1;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);

  ## With respect to scale s.
  ds_phase = diff(phase);

  ## Also compensate for wrapping with scale derivative.
  whichwrap = abs(ds_phase) > (1.5 * pi);
  phasewrap = whichwrap .* (2 * pi - abs(ds_phase));
  wrapped_ds_phase = (whichwrap == 0) .* ds_phase + phasewrap .* whichwrap;

  ## d2s_phase = diff(wrapped_ds_phase);
  most_congruent = abs(wrapped_ds_phase);
  ## cleaned, wrapped and ready to serve...
  cooked_ds_phase = cleanPhase(mag(1:nscale-1,:), wrapped_ds_phase, 0.001, 7.0);

  ## Identify cooked_ds_phase troughs across the dilation scale axis
  ## at each time point. 

  ## mid point of values closest to zero, within one std dev.

  for t=1:ntime,
    localmin = (cooked_ds_phase(dplus,t) - cooked_ds_phase (d,t)) > precision  & \
	(cooked_ds_phase(dminus,t) - cooked_ds_phase(d,t)) > precision & \
	mag(d,t) > precision;
    ## this retains the modulus peak values, clamping non maxima to zero.
    ## maxmap(:,t) = localmax .* cooked_ds_phase(:,t);
    ridges(1:nscale-1,t) = localmin;
  endfor
  
  fprintf(stderr, "Correspondance between magnitude ridge and phase derivative wrt scale\n");
  plot(cooked_ds_phase(:,500), "1", mag(1:nscale-1,500) .* 2, "2", \
       ridges(1:nscale-1,500), "3", most_congruent(:,500), "4");
  pause

endfunction  
