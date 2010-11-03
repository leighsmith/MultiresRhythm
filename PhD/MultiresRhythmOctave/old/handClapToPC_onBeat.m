### -*- Octave -*-
###
### Function to compute times to clap and how hard from the original
### rhythm and phase congruency measure.
###
### This has the limitation of only assessing PC at actual beat
### locations, rather than tactus points. Perhaps I could use both.
function [clapAt, clapIntensity] = handClapToPC_onBeat(signal, pc)
  #size(signal);
  #size(pc);

  ## determine the weighting of each beat from the phase congruency
  beatWeighting = (signal > 0.001) .* pc;

  ## and when each beat occurs
  beatLocations = find(beatWeighting);

  ## how many
  nbeats = length(beatLocations);

  ## determine the highest PC measures at beat points. 
  [ascendingWeights, weightLocations] = sort(beatWeighting);

  descendingStrengths = fliplr(ascendingWeights);
  descendingStrengthBeatLocations = fliplr(weightLocations);

  ## This is kludged, it should be something more meaningful than a proportion
  significantBeats = 1:nbeats/4;

  clapAt = descendingStrengthBeatLocations(significantBeats);

  clapIntensity = descendingStrengths(significantBeats);

endfunction
