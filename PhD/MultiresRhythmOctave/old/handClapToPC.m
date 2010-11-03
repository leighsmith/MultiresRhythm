### -*- Octave -*-
###
### Function to compute times to clap and how hard from the original
### rhythm and phase congruency measure.
function [clapAt, clapIntensity] = handClapToPC(signal, pc)
  t      = 1:length(pc);
  tplus  = shift(t, 1);
  tminus = shift(t, -1);
  ## magnitude    = abs(magnitude);
    
  ## Identify peaks across the beat vector. This will produce a binary
  ## valued vector at the beats which are maxima wrt other beats.
  pcDerivs = pc(t) > pc(tplus) & pc(t) > pc(tminus);

  ## Get the sample indexes to the beats with phase congruency derivatives
  clapAt = find(pcDerivs);

  clapIntensity = pc(clapAt);
endfunction
