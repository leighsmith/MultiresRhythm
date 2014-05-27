## -*- Octave -*-
## function to return a vector of scale numbers from periods of time 
## support (in samples)

function scales = scaleFromPeriod(timePeriods, voicesPerOctave)
  ## Highest two octaves (time domain extent 0-1,1-2) don't tell us much,
  ## so we save computation time by skipping them.
  skipInitialOctaves = 2;
  scales = voicesPerOctave .* (log2(timePeriods) - skipInitialOctaves);
endfunction
