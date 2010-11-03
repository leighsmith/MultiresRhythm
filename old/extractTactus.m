## -*- Octave -*-
##
## extractTactus -- determine from the magnitude maxima of the Continuous
## Wavelet Transform the foot-tapping rate.
## We do so by finding an unbroken ridge across the entire signal.
## This is done by a greedy-choice algorithm (p334 Cormen, Leiserson, Rivest).
## While we may extend a candidate tactus, preserving it's continuity

function tactus = extractTactus(scaleModPeaks, phase, centeredAtScale)
  maxChange = 2;		# The amount to change to be considered
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
  prefer_zero_one_indexing = 1;
  ## Extract the lowest scales, checking no times are missed.
  while(keepGoing)
    ridgeExtract = [diff(ridgeTimes); 1];
    ## When there is only 1 scale for a time point, we have rejected 
    ## candidates as much as possible.
    ## singleScaleTimes = find(count_common(ridgeTimes) == 1)
    noTactus = length(ridgeTimes) < nTime;
    if(noTactus)
      break;
    endif
    candidateTactus = scale(ridgeExtract);
    ## debug plot
    ##xd = (ridgeTimes-1);
    ##yd = nScale - scale;
    ##plot(xd+1, yd, "b.", nScale - candidateTactus, "g+");
    ##pause

    highestStep = min(candidateTactus);
    absFreqChange = abs(diff(candidateTactus));
    ## TODO need to allow for gaps when any other alternative is greater than
    ## the rejection.
    keepGoing = max(absFreqChange) > maxChange;
    if(keepGoing)
      ## Conservatively remove those with high scale differences below
      ## the highestStep by rejecting candidates either side of the
      ## portion containing the highest step.
      discontinuous = find(absFreqChange > maxChange); # time points
      lastdis = length(discontinuous)
      highestScaleTime = find(highestStep == candidateTactus);
      
      ## Only consider the first highest point.
      firstHighest = highestScaleTime(1);

      ## Determine where our highest scale lies between timespoints of
      ## discontinuous strata.
      retainIndex = insertpoint(discontinuous, firstHighest);

      ## We must handle three cases, with two subcases when the reject
      ## portions are the first or last segments.

      ## Case: -__ firstHighest precedes the first discontinuity
      ## therefore reject after from 
      ## (first discontinuity, second discontinuity) only.
      if(firstHighest <= discontinuous(1)) # empty retainIndex
	rejectBefore = 0;
	if(length(discontinuous) == 1) 	# reject first discontinuous portion
      	  rejectAfter = ridgeTimes > discontinuous(1);
	else
      	  rejectAfter = (ridgeTimes > discontinuous(1) & \
			 ridgeTimes <= discontinuous(2));
	endif

      elseif(firstHighest > discontinuous(lastdis))
      	## Case: __- firstHighest follows the last discontinuity
      	## therefore reject before the (penultimate, last) only
	rejectAfter = 0;
	rejectBefore = (ridgeTimes > discontinuous(lastdis-1) & \
			ridgeTimes <= discontinous(lastdis));

      else
      	## Case: _-_ firstHighest lies within the discontinuity time list
	if(retainIndex == 1)
      	  rejectBefore = (ridgeTimes >= 1 & ridgeTimes <= discontinuous(retainIndex));
      	else
      	  rejectBefore = (ridgeTimes > discontinuous(retainIndex - 1) & \
			  ridgeTimes <= discontinuous(retainIndex));
	  ## reject the discontinuous scales next to the retain strata.
	  ## if only one discontinuity remains, reject till the end time
	endif
	if(length(discontinuous) - retainIndex <= 2)
      	  rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
			 ridgeTimes <= nTime);
	else
      	  rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
			 ridgeTimes <= discontinuous(retainIndex + 2));
	endif
      endif

      ##printf("rejectBefore %d,%d\n", discontinuous(retainIndex - 1), \
	       ##       discontinuous(retainIndex));
      ##printf("rejectAfter %d,%d\n", discontinuous(retainIndex + 1), \
	       ##       discontinuous(retainIndex + 2));
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



















































