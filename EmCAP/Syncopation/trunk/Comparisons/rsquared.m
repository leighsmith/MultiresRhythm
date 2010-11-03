# -*- Octave -*-
# Compute coefficient of determination, also known as the proportion of explained variation.

function r_squared = rsquared(observations, model)
  ## Sum Squared Error
  sse = sum((observations - model) .^ 2)
  ## avg = mean(observations)
  avg = mean(model)
  sst = sum((observations - avg) .^ 2)
  r_squared = 1 - sse / sst
endfunction
  
