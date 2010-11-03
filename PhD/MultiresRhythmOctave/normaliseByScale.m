## -*- Octave -*-
##
## $Id$
## Normalise an analysis finding the maximum scale at each time point.
## We assume values are positive values only, such that 
function normalised = normaliseByScale(analysis)
  sz = size(analysis);
  nScale = sz(1);
  nTime = sz(2);

  maxVals = zeros(sz);
  ## Determine the maximum scale at each time.
  maxScalePerTime = max(abs(analysis));
  ## If a maximum scale value is 0, then make those 1 to keep the
  ## division healthy.
  maxScalePerTime = maxScalePerTime + !maxScalePerTime;
  ## Duplicate the maximum scales per time for each scale to enable
  ## element-wise division.
  for i = 1:nScale
    maxVals(i,:) = maxScalePerTime;
  endfor
  normalised = analysis ./ maxVals;

endfunction
