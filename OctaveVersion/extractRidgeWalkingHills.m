### -*- Octave -*-
###
### $Id:$
###
### extractRidgeWalkingHills -- determine from the correlated topography
### of modulus maxima, stationary phase and local phase congruency
### candidate ridges of the Continuous Wavelet Transform.
### We do so by finding an unbroken ridge across the entire signal.

### This really should be some sort of least squares piecewise curve
### fitting endeavour.

### Just retrieves the ridges into a structure classifying each as a
### separate, undulating ridge across time.
### Uses a distance measure that measures difference between scales and
### difference in energy magnitude.
### Uses the critical constraint that each ridge has only one scale per
### time sample.


function ridge = extractRidgeWalkingHills(correlatedProfile)

  sz     = size(correlatedProfile);
  nScale = sz(1);
  nTime  = sz(2);

  ridgePeaks = determineRidges(correlatedProfile);

  ## return the ridge as a 1 * nTime matrix holding frequencies
  ## (velocities) and phases at time points.
  ridge = zeros(1, nTime);

  ## Initialise with no active ridges.
  active = zeros(nScale, nTime);

  prevRidgeScales = [];

  ## move causally, though there is not really a biological requirement
  ## (since the wavelet is non-causal anyway).
  for currentTimeIndex = 1 : 4 # nTime
    currentRidgeScales = find(ridgePeaks(:, currentTimeIndex));

    ## Eliminate all that are the same as the previous time sample,
    ## updating the history of the "active" ridges.

    ## For all those currentRidgeScales remaining, compute the
    ## difference in scale number and height between them and n previous
    ## scales in time. 

    prevRidgesForComparison = ones(length(prevRidgeScales), length(currentRidgeScales));
    for i = 1 : length(currentRidgeScales)
      prevRidgesForComparison(:, i) = prevRidgeScales;
    endfor

    ridgeComparison = ones(length(prevRidgeScales), length(currentRidgeScales));
    for i = 1 : length(prevRidgeScales)
      ridgeComparison(i, :) = prevRidgesForComparison(i, :) .- currentRidgeScales.';
    endfor

    [stillActive, matchingRidges] = find(abs(ridgeComparison) <= 1);

    ## All those within the difference threshold become
    ## the new state of each active ridge. Update the history of those
    ## ridges.

    # debugging
    abs(ridgeComparison) <= 1
    stillActive
    matchingRidges
    currentRidgeScales(matchingRidges)

    ## find indexes of each of the elements of prevRidgeScales(stillActive)
    ## use the index permutation to update active() array.
    active(stillActive, currentTimeIndex) = currentRidgeScales(matchingRidges);

    ## Only those scales which constitute new ridges remain. Create new active
    ## ridges.

    ## Those not in matchingRidges are new ridges. Add them to the
    ## active list.

    printf("new ridges\n");
    newRidges = complement(matchingRidges, \
			   1 : length(currentRidgeScales))

    ## able to insert a row into a matrix? reshape?
    currentRidgeScales(newRidges)

    ## currentTimeIndex;

    ## Any ridges which were not updated in the previous three cases are
    ## deemed inactive and marked as such.
    
    inactiveRidges = active(:, currentTimeIndex) == 0; # 0 indicates unassigned.
    ## able to delete a row?

    # save_ridges(
    prevRidgeScales = currentRidgeScales;

  endfor

  ridge = active(1:20, 1:4);

endfunction

