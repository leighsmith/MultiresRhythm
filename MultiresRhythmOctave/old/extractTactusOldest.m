## -*- Octave -*-
##
## extractTactus -- determine from the magnitude maxima of the Continuous
## Wavelet Transform the foot-tapping rate.
## We do so by finding an unbroken ridge across the entire signal by hill walking.

## This really should be some sort of least squares piecewise curve
## fitting endeavour.

function tactus = extractTactusHills(scaleModPeaks, phase, centeredAtScale)

  sz     = size(scaleModPeaks);
  nScale = sz(1);
  nTime  = sz(2);

  ## return the tactus as a 2 * nTime matrix holding frequencies
  ## (velocities) and phases at time points.
  tactus = zeros(2, nTime);

  ## find returns each time point, ordered according to scale
  ## so we determine the complementary scales ordered by time
  [scale,c] = find(scaleModPeaks);
  scaleOrder = [scale,c];
  [s, i] = sort(scale);
  timeOrder = [s, c(i, :)];	# the index orders are both scale then time

  ## prune candidates according to tempo preference.
  ## TODO, assume to start where we know the strata is.
  lastScaleIndex = nScale - centeredAtScale;

  ## move causally, though there is not really a biological requirement
  ## (since the wavelet is non-causal anyway).
  lastTimeIndex = 1;
  while(lastTimeIndex != nTime)
    ## if we have deviated away from isochrony, or the initial estimate
    ## was not on a ridge, find the lastTimeIndex+1 in scaleOrder.
    possNewScale = scaleOrder(search(scaleOrder(:,2), lastTimeIndex), 1);
    
    ## Find the closest scale to the lastScaleIndex in scaleOrder,
    scaleDiff = abs(lastScaleIndex - possNewScale);

    closestScale = possNewScale(find(min(scaleDiff) == scaleDiff));

    ## if we are on the same scale, stay with it.
    if(closestScale != 0)
      ## decide on a tie for minimum distance. This should be
      ## perceptually bounded. But for now we just use the assumption of the
      ## higher index (smaller dilation).
      lastScaleIndex = closestScale(1);
    endif

    ## Find the least time index matching lastScaleIndex in timeOrder.
    ## returns the time points where this scale is present.
    ## scaleTimes = timeOrder(search(timeOrder(:,1), lastScaleIndex), 2)

    tactus(1, lastTimeIndex) = nScale - lastScaleIndex;
    tactus(2, lastTimeIndex) = phase(lastScaleIndex, lastTimeIndex);

    ## Add all points that remain on the same scale (isochrony) differing
    ##  by one time index from the previous using timeOrder and therefore
    ## scaleTimes
    lastTimeIndex = lastTimeIndex + 1; # TODO more general than this

  endwhile
  tactus(1, lastTimeIndex) = nScale - lastScaleIndex;
  tactus(2, lastTimeIndex) = phase(lastScaleIndex, lastTimeIndex);

endfunction
