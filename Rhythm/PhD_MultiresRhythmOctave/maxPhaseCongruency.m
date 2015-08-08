## -*- Octave -*-
## find maximum phase congruency
function [whereTopN, topN] = maxPhaseCongruency(signal, pc)
  [whereTopN, topN] = highest(pc, 5);
endfunction
