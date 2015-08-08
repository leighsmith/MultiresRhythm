# -*- Octave -*-
# Function to determine
function IntervalTimes = peakRates(magnitude, time, voicesPerOctave, sr)
  if(nargin < 3)
    voicesPerOctave = 8;
  endif
  if(nargin < 4)
    sr = 200;
  endif
  # determine the peaks at scales
  # we should compute this for magnitude(:, time)
  inflections = [8,16,22,28,31];
  numScales = rows(magnitude);
  IntervalSamples = timeSupport(numScales - inflections, voicesPerOctave)
  IntervalTimes = IntervalSamples ./ sr
endfunction
