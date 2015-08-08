### -*- Octave -*-
###
### 
function ridges = determineRidges(correlatedProfile)

  correlation_minimum = 0.01;

  sz = size(correlatedProfile);
  nscale = sz(1);
  ntime = sz(2);
  ridges = zeros(sz);
  
  ## identify peaks in the combined correlation of energy modulus,
  ## stationary phase and local phase congruency across the dilation
  ## scale axis at each time point.
  d      = 1:nscale;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);

  for t = 1 : ntime,
    localmax = correlatedProfile(d, t) > correlatedProfile(dplus, t) & \
	correlatedProfile(d, t) > correlatedProfile(dminus, t) & \
	correlatedProfile(d, t) > correlation_minimum;
    ## this retains the modulus peak values, clamping non maxima to zero.
    ridges(:, t) = localmax .* correlatedProfile(:, t);
  endfor

endfunction
