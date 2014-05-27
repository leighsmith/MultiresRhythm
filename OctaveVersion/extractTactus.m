## -*- octave -*-
##
## $Id$
## extractTactus -- determine the foot-tapping rate from the magnitude
## maxima of the Continuous Wavelet Transform.
## We do so by finding an unbroken ridge across the entire signal.
## This is done by a greedy-choice algorithm (p334 Cormen, Leiserson, Rivest),
## extending a candidate tactus, preserving it's continuity by
## rejecting discontinuous regions.
## Debug output convention is that ( indicates an open interval, ] closed.

##            maxAcceptableChange
## hyperbolic          2
## hon-un              1
## honing-unaccented   2
## greensleeves_accent 2          rd
## greensleeves_accent 2          rd | rlp
## anapestics          2
function tactus = extractTactus(scaleModPeaks)
  global promptStream;		# Where we send prompts to.
  debug_stream = promptStream;	# To watch algorithm in realtime or later
  prefer_zero_one_indexing = 1; # To make sure predicate indexing works
  maxAcceptableChange = 2;	# The amount to change to be considered
				# a different scale. \Delta s in thesis.
  debug_plot = 0;               # to plot the progress of the algorithm


  sz     = size(scaleModPeaks);
  nScale = sz(1);
  nTime  = sz(2);

  ## return the tactus as a nTime matrix holding frequencies
  ## (velocities) and phases at time points.
  tactus = zeros(1, nTime);

  ## in case we break with no tactus immediately.
  candidateTactus = zeros(nTime, 1);

  ## find returns each time point, ordered according to scale
  ## This will quantize any continuous valued ridges to 1/0.
  [scale,ridgeTimes] = find(scaleModPeaks);

  ## debug plot
  if(debug_plot)
    xd = (ridgeTimes-1);
    yd = nScale - scale;
##    plot(xd+1, yd, "b.", nScale - candidateTactus, "g+", firstHighest, nScale - highestStep, "rx");
    plot(xd+1, yd, "r.");
    pause
  endif

  keepGoing = 1;
  while(keepGoing)
    ## Extract the lowest scales, checking no times are missed.
    ## diff will give us a 1 at the time index transitions, 
    ## 0 for time indexes which are the same.
    ## Tack on an extra time at the end after the last time to
    ## generate a full set of time differences and catch the case
    ## where there are no ridges on the last time point.
    ridgeExtract = diff([ridgeTimes; nTime+1]);

    ## When there is less than 1 scale per time point, the maximum
    ## time difference will be > 1. We have then rejected candidates
    ## as much as possible without finding a tactus.
    maxTimeDiff = max(ridgeExtract);
    noTactus = maxTimeDiff > 1;    # flag to return
    if(noTactus)
      ridgeLost = ridgeTimes(maxTimeDiff == ridgeExtract) + 1;
      fprintf(debug_stream, "Error: Couldn't determine tactus, no ridges left @ sample %d\n", ridgeLost);
      break;
    endif

    candidateTactus = scale(find(ridgeExtract));

    ## lowest numbered scales are highest frequencies.
    highestStep = min(candidateTactus);
    absFreqChange = abs(diff(candidateTactus));

    keepGoing = max(absFreqChange) > maxAcceptableChange;
    if(keepGoing)
      ## Conservatively remove those with high scale differences below
      ## the highestStep by rejecting candidates either side of the
      ## portion containing the highest step.

      ## Holds the time points of the discontinuities. 
      ## Pad either side with last possible times if they aren't there already
      discontinuous = create_set([1; find(absFreqChange > maxAcceptableChange); nTime]);

      lastdis = length(discontinuous);
      timeOfHighestScale = find(highestStep == candidateTactus);
      
      ## Only consider the first highest point.
      firstHighest = timeOfHighestScale(1);
      
      ## Determine where our highest scale lies between time-points of
      ## discontinuous strata.
      retainIndex = insertpoint(discontinuous, firstHighest);

      ## debug dump of variables
      ## firstHighest
      ## lastdis
      ## retainIndex
      ## discontinuous

      ## kludge escaping when 1 is discontinuous and the only other
      ## index is the end time.
      if(lastdis <= 2)
        break;
      endif

      ## We must handle three cases.

      if(retainIndex == 1)
	fprintf(debug_stream, "Preceding:    -__ reject from (%d,%d]\n",
		discontinuous(2), discontinuous(3));
        ## firstHighest is within the first and second discontinuity
        ## therefore reject after from 
        ## (second discontinuity, third discontinuity) only.
	rejectBefore = 0;
      	rejectAfter = (ridgeTimes > discontinuous(2) & \
	               ridgeTimes <= discontinuous(3));
      elseif(retainIndex == lastdis-1 || retainIndex == lastdis)
	fprintf(debug_stream, "Following:    __- reject from (%d,%d]\n",
		discontinuous(lastdis-2), discontinuous(lastdis-1));
      	## firstHighest is between the penultimate and last discontinuity
      	## therefore reject between the (third-last, second-last) only.
        ## We don't reject the actual third-last discontinuity as that
        ## marks the end of the next discontinuity.
        ## Unless it's the first time.
        rejectBefore = (ridgeTimes > discontinuous(lastdis-2) & \
			ridgeTimes <= discontinuous(lastdis-1)) | \
                       (discontinuous(lastdis-2) == 1 & ridgeTimes == 1);
	rejectAfter = 0;
      else
      	## firstHighest lies within the discontinuity time list,
	## reject the discontinuous scales next to the retain strata.

        ## if the highest scale is actually just a single point,
        ## retain it, otherwise retainIndex has the preceding discontinuity.
        if(firstHighest == discontinuous(retainIndex))
          fprintf(debug_stream, "Median Point: _._ ");
	  if(retainIndex == 2)
	    before1 = retainIndex - 1;
            before2 = retainIndex - 1;
          else
	    before1 = retainIndex - 2;
            before2 = retainIndex - 1;
          endif
          after1 = retainIndex;
          after2 = retainIndex + 1;
        else
          fprintf(debug_stream, "Median:       _-_ ");
	  before1 = retainIndex - 1;
          before2 = retainIndex;
          after1 = retainIndex + 1;
          after2 = retainIndex + 2;
        endif
        ## reject the ridge portion before the retain portion, but
        ## don't reject the first discontinuity point of the ridge
        ## portion as it marks the end of the preceding portion, unless
        ## it's 1.
        rejectBefore = (ridgeTimes > discontinuous(before1) & \
                        ridgeTimes <= discontinuous(before2)) | \
                       (discontinuous(before1) == 1 & ridgeTimes == 1); 

        ## reject after (not including) the next discontinuous time
        ## point upto (and including) the discontinuous time point
        ## following that.
        rejectAfter = (ridgeTimes > discontinuous(after1) & \
	               ridgeTimes <= discontinuous(after2));

        fprintf(debug_stream, "reject (%d,%d] and (%d,%d]\n",
        	discontinuous(before1), discontinuous(before2),
        	discontinuous(after1), discontinuous(after2));
      endif

      ## debug plot, but only when there is something worth seeing
      if(debug_plot)
        reject = (rejectBefore | rejectAfter) & ridgeExtract;
        xd = (ridgeTimes-1);
        yd = nScale - scale;
        plot(xd+1, yd, "r.", nScale - candidateTactus, "b+",
             ridgeTimes(reject), nScale - scale(reject),  "mx",
             firstHighest, nScale - highestStep, "gx");
        ##plot(nScale - candidateTactus, "b+",
        ##     ridgeTimes(reject), nScale - scale(reject),  "mx",
        ##     firstHighest, nScale - highestStep, "gx");
        debug_plot = 1;
        pause
      endif

      retain = !((rejectBefore | rejectAfter) & ridgeExtract);
      scale = scale(retain);
      ridgeTimes = ridgeTimes(retain);
    endif
  endwhile

  tactus = candidateTactus.';

  if(!noTactus)
    fprintf(debug_stream, "Extracted tactus\n");
  endif
endfunction
