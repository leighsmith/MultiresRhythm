## -*- octave -*-
##
## extractTactus -- determine from the magnitude maxima of the Continuous
## Wavelet Transform the foot-tapping rate.
## We do so by finding an unbroken ridge across the entire signal.
## This is done by a greedy-choice algorithm (p334 Cormen, Leiserson, Rivest).
## While we may extend a candidate tactus, preserving it's continuity

function tactus = extractTactusOrig(scaleModPeaks, phase, centeredAtScale)
  prefer_zero_one_indexing = 1; # To make sure predicate indexing works
  maxAcceptableChange = 2;	# The amount to change to be considered
				# a different scale. \delta s in thesis.

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
  while(keepGoing)
    ridgeExtract = [diff(ridgeTimes); 1];
    ## When there is less than 1 scale per time point, the maximum
    ## time difference will be > 1. We have then rejected candidates
    ##  as much as possible without finding a tactus.
    noTactus = (max(ridgeExtract) > 1);
    if(noTactus)
      break;
    endif
    candidateTactus = scale(ridgeExtract);

    ## debug plot
    ## xd = (ridgeTimes-1);
    ## yd = nScale - scale;
    ## plot(xd+1, yd, "b.", nScale - candidateTactus, "g+");
    ## pause

    highestStep = min(candidateTactus);
    absFreqChange = abs(diff(candidateTactus));
    ## TODO try need to allow for gaps when any other alternative 
    ## is greater than the rejection.
    keepGoing = max(absFreqChange) > maxAcceptableChange;
    if(keepGoing)
      ## Conservatively remove those with high scale differences below
      ## the highestStep by rejecting candidates either side of the
      ## portion containing the highest step.
      discontinuous = find(absFreqChange > maxAcceptableChange); # time points
      lastdis = length(discontinuous);
      highestScaleTime = find(highestStep == candidateTactus);
      
      ## Only consider the first highest point.
      firstHighest = highestScaleTime(1);

      ## Determine where our highest scale lies between time-points of
      ## discontinuous strata.
      retainIndex = insertpoint(discontinuous, firstHighest);

      ## We must handle three cases, with two subcases when the reject
      ## portions are the first or last segments.

      ## debug dump of variables
      ## retainIndex
      ## firstHighest
      ## lastdis
      ## discontinuous

      if(firstHighest <= discontinuous(1)) # empty retainIndex
	printf("Case: -__\n");
        ## firstHighest precedes the first discontinuity
        ## therefore reject after from 
        ## (first discontinuity, second discontinuity) only.
	rejectBefore = 0;
	if(lastdis == 1) 	# reject first discontinuous portion
      	  rejectAfter = ridgeTimes > discontinuous(1);
	else
      	  rejectAfter = (ridgeTimes > discontinuous(1) & \
			 ridgeTimes <= discontinuous(2));
	endif

      elseif(firstHighest >= discontinuous(lastdis))
	printf("Case: __-\n");
      	## firstHighest follows the last discontinuity
      	## therefore reject before the (penultimate, last) only
	rejectAfter = 0;
	if(lastdis == 1)
	  rejectBefore = ridgeTimes >=  1 & ridgeTimes <= discontinuous(1);
	else
	  rejectBefore = (ridgeTimes > discontinuous(lastdis-1) & \
			  ridgeTimes <= discontinuous(lastdis));
	endif
      else
        printf("Case: _-_\n");
      	## firstHighest lies within the discontinuity time list,
	## reject the discontinuous scales next to the retain strata.
	## If only one discontinuity remains, reject until the end time
	if(retainIndex == 1)
      	  rejectBefore = (ridgeTimes >= 1 & ridgeTimes <= discontinuous(retainIndex));
      	else
          ## if the highest scale is actually just a single point,
          ## retain it, otherwise retainIndex has the preceding discontinuity.
	  if(firstHighest == discontinuous(retainIndex))
	    rejectBefore= (ridgeTimes > discontinuous(retainIndex - 2) & \
		            ridgeTimes <= discontinuous(retainIndex - 1));
	  else
      	    rejectBefore = (ridgeTimes > discontinuous(retainIndex - 1) & \
		            ridgeTimes <= discontinuous(retainIndex));
          endif
	endif
        ##
	if(lastdis - retainIndex <= 2)
      	  rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
			 ridgeTimes <= nTime);
	else
      	  rejectAfter = (ridgeTimes > discontinuous(retainIndex + 1) & \
			 ridgeTimes <= discontinuous(retainIndex + 2));
	endif
      endif

      ## printf("rejectBefore %d,%d\n", discontinuous(retainIndex - 1), \
	##              discontinuous(retainIndex));
      ## printf("rejectAfter %d,%d\n", discontinuous(retainIndex + 1), \
	 ##             discontinuous(retainIndex + 2));
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



















