%%% Calculates CWT on 1-D signal.
%%% [magnitude, phase] = cwt(signal, voicesPerOCtave, maxWaveletPeriod)
%%% The continuous wavelet transform operates on 1xN vector, producing
%%% 2-D magnitude and phase values for a given resolution per octave to a maximum
%%% wavelet period (must be dyadic).
function [magnitude, phase] = cwt(signal, voicesPerOctave, maxWaveletPeriod)
  %% Pads the signal to a dyadic length, then calls dyadic_cwt(),
  %% trimming off the returned result.
  [pad_signal, trim] = dyadicPad(signal);

  [padMagnitude, padPhase] = dyadic_cwt(pad_signal, voicesPerOctave, maxWaveletPeriod);

  magnitude = padMagnitude(:, trim);
  phase = padPhase(:, trim);
endfunction
%%% -*- Octave -*-
