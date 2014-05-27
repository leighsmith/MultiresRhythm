## -*- Octave -*-
##
## extractTactus -- determine from the magnitude maxima of the Continuous
## Wavelet Transform the foot-tapping rate.
## We do so by finding an unbroken ridge across the entire signal.
## This is done by a greedy-choice algorithm (p334 Cormen, Leiserson, Rivest).
## While we may extend a candidate tactus, preserving it's continuity

function tactus = extractTactus(scaleModPeaks, phase, centeredAtScale)
  prefer_zero_one_indexing = 1; # To make sure predicate indexing works
  maxAcceptableChange = 2;	# The amount to change to be considered
				# a different scale.

  sz     = size(scaleModPeaks);
  nScale = sz(1);
  nTime  = sz(2);

  ## return the tactus as a 2 * nTime matrix holding frequencies
  ## (velocities) and phases at time points.
  tactus = zeros(2, nTime);

  ## find returns each time point, ordered according to scale
  [scale,ridgeTimes] = find(scaleModPeaks);

  keepGoing = 1;
  ## Extract the lowest scales, checking no times are missed.
  ## When there is only 1 scale for a time point, we have rejected 
  ## candidates as much as possible.
  ## singleScaleTimes = find(count_common(ridgeTimes) == 1)
  while(keepGoing && \
	(noTactus = (max(ridgeExtract = [diff(ridgeTimes); 1]) <= 1)))
    candidateTactus = scale(ridgeExtract)
    ## debug plot
    ##xd = (ridgeTimes-1);
    ##yd = nScale - scale;
    ##plot(xd+1, yd, "b.", nScale - candidateTactus, "g+");
    ##pause

    absFreqChange = abs(diff(candidateTactus));
    ## need to allow for gaps when any other alternative is greater than
    ## the rejection.
    keepGoing = max(absFreqChange) > maxAcceptableChange;
    if(keepGoing)
      ## Conservatively remove those with high scale differences below
      ## the highestStep by rejecting candidates either side of the
      ## portion containing the highest step.
      discontinuous = find(absFreqChange > maxAcceptableChange) # time points
      highestScaleTime = find(min(candidateTactus) == candidateTactus);
      
      ## Only consider the first highest point.
      firstHighest = highestScaleTime(1)

      ## Determine where our highest scale lies between timespoints of
      ## discontinuous strata.
      retainIndex = insertpoint(discontinuous, firstHighest)
      ## empty retainIndex implies firstHighest lies before or after the
      ## discontinuity, therefore reject after or before the first point
      ## respectively.
      if(length(retainIndex) == 0)
	if(firstHighest > discontinuous(1))
	  rejectBefore = ridgeTimes >= 1 & ridgeTimes <= discontinuous(1); 
	else
	  retainIndex = 0;	# reject first discontinuous portion instead
	  rejectBefore = 0;
	endif
      elseif(retainIndex == 1)
      	rejectBefore = ridgeTimes <= discontinuous(retainIndex) & ridgeTimes >= 1;
      else
      	rejectBefore = (ridgeTimes > discontinuous(retainIndex - 1) & \
			ridgeTimes <= discontinuous(retainIndex));
      endif
      ##printf("rejectBefore %d,%d\n", discontinuous(retainIndex - 1), \
	       ##       discontinuous(retainIndex));
      ## reject the discontinuous scales next to the retain strata.
      ## if only one discontinuity remains, reject till the end time
      if(length(discontinuous) - retainIndex <= 2)
      	rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
		       ridgeTimes <= nTime);
      else
      	rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
		       ridgeTimes <= discontinuous(retainIndex + 2));
      endif
      ##printf("rejectAfter %d,%d\n", discontinuous(retainIndex + 1), \
	##              discontinuous(retainIndex + 2));
      retain = !((rejectBefore | rejectAfter) & ridgeExtract);
      scale = scale(retain);
      ridgeTimes = ridgeTimes(retain);
    endif

  endwhile

  if(noTactus)
    printf("Error: Couldn't determine tactus\n");
  endif

  tactus(1, :) = nScale - candidateTactus.';
  tactus(2, :) = 0.5;		# TODO kludged

endfunction



















