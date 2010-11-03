### -*- Octave -*-
###
### Function to compute times to clap and how hard from the original
### rhythm and phase congruency measure.
function [clapAt, clapIntensity] = handClap(signal, pc)
  #size(signal);
  #size(pc);

  ## determine the weighting of each beat 
  beatWeighting = (signal > 0.001) .* pc;

  ## and when each beat occurs
  beatLocations = find(beatWeighting);

  ## how many
  nbeats = length(beatLocations);

  ## compact the weighting at each beat into vector
  beats = beatWeighting(beatLocations);

  d      = 1:nbeats;
  dplus  = shift(d, 1);
  dminus = shift(d, -1);
  ## magnitude    = abs(magnitude);
    
  ## Identify peaks across the beat vector. This will produce a binary
  ## valued vector at the beats which are maxima wrt other beats.
  pcDerivs = beats(d) > beats(dplus) & beats(d) > beats(dminus);

  ## Get the sample indexes to the beats with phase congruency derivatives
  clapAt = beatLocations(find(pcDerivs));

  clapIntensity = pc(clapAt);
endfunction
