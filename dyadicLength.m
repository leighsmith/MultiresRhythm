### -*- Octave -*-
###
### $Id:$
###
### Returns the length of the signal padded to a dyadic (power of two) length.
###
function paddedLength = dyadicLength(signal_length)
  paddedLength = 2^ceil(log2(signal_length));
endfunction
