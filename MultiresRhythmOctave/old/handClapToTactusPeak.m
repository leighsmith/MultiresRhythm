### -*- Octave -*-
###
### Function to compute times to clap and how hard from the extracted tactus.
### Returns a matrix where row 1 is the time to tap at, row 2 is the intensity.

function [claps, footTapAM] = handClapToTactusPeak(signal, mag, phase, tactusIndices, \
					       voicesPerOctave, startFrom)
  sz = size(mag);
  nScale = sz(1);
  nTime = sz(2);

  ## this assumes there is a single scale index per time in tactusIndices
  fortran_indexes = ((0:nTime-1) .* nScale) + tactusIndices;

  ## extract out the magnitude and phase
  fortran_behaviour = do_fortran_indexing;
  tactusPeak = zeros(sz);
  constantMag = ones(sz);
  do_fortran_indexing = 1;
  ## tactusMag(fortran_indexes) = mag(fortran_indexes);
  ## really we should be able to do this:
  ## tactusMag(fortran_indexes) = 1;

  tactusPeak(fortran_indexes) = constantMag(fortran_indexes);
  ## The right way to do this is with a tight (well, two octave)
  ## Gaussian envelope over it.
  ##tactusMag = gaussCentered(tactusPeak, voicesPerOctave);
  ## but just a single voice alone produces the correct oscillation,
  ## with lower amplitude.
  tactusMag = tactusPeak;
  do_fortran_indexing = fortran_behaviour;
  
  cwt_plot(tactusMag(:,1:4:nTime), phase(:,1:4:nTime));

  ## pad the magnitude and phase 
  [padTactusMag, trimTactusMag] = dyadicPad(tactusMag);
  [padTactusPhase, trimTactusPhase] = dyadicPad(phase);


  ## To be really correct the original padding from dyadic_cwt should be used
  ## but the effect is subtle and it's a pain to use globals or hand
  ## around a padded version of the CWT output.
  ## global padMagnitude;
  ## global padPhase;
  ## padTactusPhase = padPhase;

  ## use them to compute a sinusoid
  footTapAM = real(dyadic_icwt(padTactusMag, padTactusPhase, voicesPerOctave));

  ## clean off the padding
  footTapAM = footTapAM(:,trimTactusMag);

  ## The peak time point of the oscillating sinusoid is the beat location.
  t      = 1:nTime;
  tplus  = shift(t, 1);
  tminus = shift(t, -1);
    
  ## identify peaks across the translation (time) axis
  localmax = footTapAM(t) > footTapAM(tplus) & \
      footTapAM(t) > footTapAM(tminus) & footTapAM(t) > 1.0e-4;
  clapAt = find(localmax);
  nClaps = length(clapAt);

  claps = zeros(2, nClaps);
  claps(1,:) = clapAt;
  ## use constant intensity claps but weight the amplitude for when we
  ## mix in Common Music.
  claps(2,:) = ones(1,nClaps) * 0.6;

endfunction

