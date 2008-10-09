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

function ridges = ridgesStatPhase(mag,phase,par)
  sz = size(phase);
  nscale = sz(1);
  ntime = sz(2);
  
  ridges = zeros(sz);

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
  wrapped_dt_phase = (wrapped_dt_phase > -1) .* wrapped_dt_phase;

  ## Need to ensure the derivative of the phase reveals the frequency
  ## at the given scale. How?
  ## If omega0 gives the time support of the mother wavelet, 
  ## then dividing the original omega0 by the time support of the
  ## scaled version gives the dilation number a (on the b,a plane).
  ## as the phase indicates the time support of the frequency
  ## (time intervals between pi/-pi crossings)

  ## take second derivative of the cleaned up phase.
  d2_t_phase = diff(wrapped_dt_phase.').';

  ## With respect to scale s.
  ds_phase = diff(phase);

  ## Also compensate for wrapping with scale derivative.
  whichwrap = abs(ds_phase) > (1.5 * pi);
  phasewrap = whichwrap .* (2 * pi - abs(ds_phase));
  wrapped_ds_phase = (whichwrap == 0) .* ds_phase + phasewrap .* whichwrap;
  ## cleaned, wrapped and ready to serve...
  cooked_ds_phase = cleanPhase(mag(1:nscale-1,:), wrapped_ds_phase, 0.001, 7.0);

  previousRidgeScale = nscale / 2; # each-way bet
  omega0 = 6.2;    # must be matched with MorletWaveletFourier.m
  ## check the derivative of the phase is not zero...
  divisiblePhaseDeriv = wrapped_dt_phase + (abs(wrapped_dt_phase) < 1.0e-9) * 1.0e+9;
  ## problem, this seems to produce higher scale number than legal.
  ## derivative of the phase should be producing omega1 defined in 6.5
  r_scale = nscale - (omega0 ./ divisiblePhaseDeriv);
  r_scale = clampToBounds(r_scale, r_scale, -10, -10, nscale, nscale);
##  for i=1:5   # iterate until convergence
##    ridgePoint = r_scale(previousRidgeScale,1)
##    previousRidgeScale = ridgePoint
##  end

  ridge_diff = diff(wrapped_dt_phase);

  ## differentiation in both planes reduces the result by one row/column
  ## so we insert back into the matrix as neccessary.
  ##ridges(:,1:ntime-1) = abs(dt_phase) > pi;
  ##ridges(:,1:ntime-1) = wrappedphase == 0;
  ##ridges(:,1:ntime-2) = (d2_t_phase > 0.001);
  ridges(1:nscale-1,1:ntime-1) = ridge_diff > 0.001;

  ##tosave = wrappedphase.';
  ##save -ascii /tmp/wrappedphase.txt tosave
  ##grayscale = colormap("default");
  ##imagesc(wrapped_dt_phase,1);

  dbug_scale = 50;  # 1 is highest frequency scale, nscale lowest
  ## timeport = 75:135;
  timeport = 1:ntime-3;
  ##timeport = 832:852; 
  ## plot(phase(dbug_scale,timeport),"1",\
	  ##      dt_phase(dbug_scale,timeport),"2",\
	  ##      mag(dbug_scale,timeport),"3")
  plot(mag(dbug_scale,timeport) .* phase(dbug_scale,timeport),"1",\
       mag(dbug_scale+5,timeport) .* phase(dbug_scale+5,timeport),"2",\
       mag(dbug_scale-5,timeport) .* phase(dbug_scale-5,timeport),"3")
  pause

  fprintf(stderr, "phase wrap interval\n");
  plot(phase_wrap_interval(dt_phase, dbug_scale),"1",\
       phase_wrap_interval(dt_phase, dbug_scale+5),"2",\
       phase_wrap_interval(dt_phase, dbug_scale-5),"3");

  ##plot(r_scale(dbug_scale,timeport),"3")
  ##gset hidden3d
  ## gsplot wrappedphase;
  pause
  fprintf(stderr, "wrapped_dt_phase\n");
  plot(wrapped_dt_phase(dbug_scale,timeport), "1", \
       wrapped_dt_phase(dbug_scale+5,timeport), "2", \
       wrapped_dt_phase(dbug_scale-5,timeport), "3", \
       (d2_t_phase(dbug_scale,timeport) > 0) .* 0.04, "4")
  pause
  ## Ok, no need to display this, we know the dt_phase glitch is
  ## happening lower than the magnitude
  ##  plot(wrapped_dt_phase(:,100), "1",\
	   ##       mag(1:nscale-1,100), "1",\
	   ##       wrapped_dt_phase(:,1024), "2",\
	   ##       mag(1:nscale-1,1024), "2",\
	   ##       wrapped_dt_phase(:,2048), "3",\
	   ##       mag(1:nscale-1,2048), "3")
  ##  pause
  ##fprintf(stderr, "Correspondance between magnitude ridge and phase derivative wrt scale\n");
  ##plot(cooked_ds_phase(:,500), "1",\
  ##     mag(1:nscale-1,500) .* 2, "2",\
  ##     wrapped_dt_phase(:,500),"3");
  ##pause
  ## No need to display this, the glitches correspond to the transition
  ## to a lack of meaningful magnitude, when the phase goes mad.
  ## plot(wrapped_dt_phase(:,50),"4",\
  ##     wrapped_dt_phase(:,52),"2",\
  ##     wrapped_dt_phase(:,100),"2",\
  ##     wrapped_dt_phase(:,500),"3",\
  ##     wrapped_dt_phase(:,1024),"1",\
  ##     wrapped_dt_phase(:,2048),"4",\
  ##     wrapped_dt_phase(:,3800),"1");
  ##pause
  ##plot(wrapped_dt_phase(:,2048), "1",ridge_diff(:,2048),"2")

endfunction  

