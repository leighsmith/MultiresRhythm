## -*- octave -*-
##
## $Id$
## Produce a weighting matching the analysis window using tempo preference.
##
function tempoWeightingOverTime = preferredTempoWeighting(magnitude, phase, voicesPerOctave, sampleRate)

  sz = size(magnitude);
  nscale = sz(1);
  ntime = sz(2);
  tempoWeightingOverTime = zeros(sz);

  ## Scale index 1 is the highest frequency (smallest dilation) scale.
  salientScale = preferredTempo(magnitude, phase, voicesPerOctave, sampleRate);
  printf("preferred tempo scale = %d\n", salientScale);

  tempoScaleWeighting = shift(gauss(nscale), salientScale - nscale ./ 2);
  for scale = 1:nscale,
		tempoWeightingOverTime(scale,:) = tempoScaleWeighting(scale);
  endfor
  figure(1);
  title("Preferred tempo weighting profile");
  plot(tempoScaleWeighting(1,:));
endfunction
