## -*- Octave -*-
## Find the highest n most values in the vector.
function [whereTopN, topN] = highest(vector, n)
  [highest, whichHighest] = sort(vector);
  max = length(vector);

  topN = highest(max - n + 1: max);
  whereTopN = whichHighest(max - n + 1: max);
endfunction
