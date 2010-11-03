### -*- Octave -*-
###
### $Id:$
###
### Pads the signal to a dyadic length, then calls dyadic_cwt(),
### trimming off the returned result.
###
function [magnitude, phase] = cwt(signal, voicesPerOctave, maxWaveletPeriod)
  ## The wavelet transform operates on 1xN vector
  [pad_signal, trim] = dyadicPad(signal);

  [padMagnitude, padPhase] = dyadic_cwt(pad_signal, voicesPerOctave, maxWaveletPeriod);

  magnitude = padMagnitude(:, trim);
  phase = padPhase(:, trim);
endfunction
