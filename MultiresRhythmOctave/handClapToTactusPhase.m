### -*- Octave -*-
###
### $Id$
###
### Function to compute times to clap and how hard from the extracted tactus.
### Returns a matrix where row 1 is the time to tap at, row 2 is the intensity.

function [claps, footTapPhase] = \
      handClapToTactusPhase(signal, mag, phase, tactusIndices, \
			    voicesPerOctave, startFrom)
  sz = size(mag);
  nScale = sz(1);
  nTime = sz(2);

  ## this assumes there is a single scale index per time in tactusIndices
  fortran_indexes = ((0:nTime-1) .* nScale) + tactusIndices;

  ## extract out the magnitude and phase
  tactusPeak = zeros(sz);
  constantMag = ones(sz);
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
  
  ## debugging plot (downsampled)
  ## plotCWT(tactusMag(:,1:4:nTime), phase(:,1:4:nTime));

  ## pad the magnitude and phase 
  [padTactusMag, trimTactusMag] = dyadicPad(tactusMag);
  [padTactusPhase, trimTactusPhase] = dyadicPad(phase);

  ## To be really correct the original padding from dyadic_cwt should be used
  ## but the effect is subtle and it's a pain to use globals or hand
  ## around a padded version of the CWT output.
  ## global padMagnitude;
  ## global padPhase;
  ## padTactusPhase = padPhase;

  ## use them to compute a sinusoid and it's Hilbert transform, from
  ## that, determine the phase of the sinusoid.
  footTapPhase = arg(dyadic_icwt(padTactusMag, padTactusPhase, voicesPerOctave));

  ## clean off the dyadic padding
  footTapPhase = footTapPhase(:,trimTactusMag);

  ## Note the phase of the oscillating sinusoid at the beat to start
  ## tapping from.
  beatPositions = find(signal);
  downBeat = beatPositions(startFrom);
  clapOnPhaseDatum = footTapPhase(downBeat);

  printf("Handclapping from beat %d of original rhythm, sample %d\n", \
	 startFrom, downBeat);

  ## check clapOnPhaseDatum >= current and < next phase measure.
  ## this could be a problem on the last phase point before 2 pi wrap.
  t      = 1:nTime;
  tminus  = shift(t, -1);
    
  ## identify reoccurance of the initial clap phase across the translation
  ## (time) axis
  reoccur = clapOnPhaseDatum >= footTapPhase(t) & clapOnPhaseDatum < footTapPhase(tminus);
  clapAt = find(reoccur);
  nClaps = length(clapAt)

  claps = zeros(2, nClaps);
  claps(1,:) = clapAt;
  ## use constant intensity claps but weight the amplitude for when we
  ## mix in Common Music.
  claps(2,:) = ones(1,nClaps) * 0.6;

endfunction

