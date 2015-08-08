## -*- Octave -*-
## function to return a vector of periods of time support (in samples)
## from scale numbers

function period = timeSupport(scales, waveletsPerOctave)
  ## Highest two octaves (time domain extent 0-1,1-2) don't tell us much,
  ## so we save computation time by skipping them.
  skipInitialOctaves = 2;
  period = 2.0 .^ (scales ./ waveletsPerOctave + skipInitialOctaves);
endfunction
