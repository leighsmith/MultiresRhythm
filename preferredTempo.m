## -*- Octave -*-
##
## Determine the scale which Fraisse's spontaneous tempo would occur at.
## This is weighted by absolute constraints, look in the 600ms period range.
##
function salientScale = preferredTempo(scaleModPeaks, phase, voicesPerOctave, rhythmSampleRate)
  sz     = size(scaleModPeaks);
  nScale = sz(1);
  nTime  = sz(2);

  ## Fraisse's "spontaneous tempo" interval in seconds
  tempoSaliencePeak = 0.600;
  salientIOI = rhythmSampleRate * tempoSaliencePeak

  ## Convert salientIOI into the scale to begin checking.
  ## This assumes the highest frequency scale (shortest time support)
  ## is index 1, lowest frequency (longest time support) is nScale.
  salientScale = floor(scaleFromPeriod(salientIOI, voicesPerOctave));
endfunction
