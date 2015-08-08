## -*- Octave -*-
## Clip a signal according to the bounds given, by testing another
## signal (which can be the same as signal). Anything clipped will be
## set to the clampLow, clampHigh values
function clamped = clampToBounds(signal,testSignal,lowbound,clampLow,highbound,clampHigh)
  sz = size(signal);
  abovelow = testSignal > lowbound;
  if(nargin < 5)
    belowhigh = ones(sz(1), sz(2));
    clampHigh = 0;    # just so long as it isn't undefined
  else
    belowhigh = testSignal < highbound;
  endif
  clamped = ((abovelow & belowhigh) .* signal) + \
    (!abovelow .* clampLow) + (!belowhigh .* clampHigh);

endfunction
