## -*- Octave -*-
## Plot a cross-section of the magnitude at a given time point so we
## can spot the highest activated scales.
function plotScaleEnergy(magnitude, time)
  voicesPerOctave = 8;
  numScales = rows(magnitude)
  numOctaves = numScales/voicesPerOctave
##  labels(1:2:2 .* (numOctaves+1)) = num2str(2 .^ ((numOctaves:-1:0) + 2));
##  labels(2:2:2 .* (numOctaves+1)) = 1:voicesPerOctave:numScales+1;
##  labels

##  axis([0 1 2 (noctave+skipInitialOctaves)]);
  xlabel('Scale as IOI Range in Samples')
  ylabel('Energy')
##  gset xtics ("abc" 1, "def" 20, "ggg" 30)
  plot(flipud(magnitude(:, time)), "b^")
endfunction
