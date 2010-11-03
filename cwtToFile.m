## -*- Octave -*-
## $Id$
##
## Function to perform a continuous wavelet transform on a signal (1xN) and
## write the signal, the result (separated as magnitude and phase)
## and the phase congruency to named files suitable for plotting with
## Mathematica. The files are saved with the smallest scales (highest
## frequencies) as the lowest numbered rows in the output file.
##
function [magnitude, phase, pc] = cwtToFile(signal, voicesPerOctave, maxWaveletPeriod, filename, doPlot)

  ## by default (or if -1 is passed in, in order to specify a filename
  ## but not a maxWaveletPeriod) we do as many scales as resolution allows.
  if(nargin < 3 || maxWaveletPeriod == -1)
    maxWaveletPeriod = dyadicLength(length(signal)) ./ 4;
  endif

  ## The wavelet transform operates on 1xN vector
  [magnitude, phase] = cwt(signal, voicesPerOctave, maxWaveletPeriod);

  if(nargin > 4 && doPlot)
    ## Plot the downsampled magnitude and phase, clamping the magnitude so
    ## we can look at local maxima.
    plotCWT("cwt", magnitude, phase); # 0.1 clamping
  endif

  ## Do phase congruency without including the congruency contributions
  ## from the impulse. 
  sz     = size(magnitude);
  nScale = sz(1);
  nTime  = sz(2);

  ## calculate PC only over the range below the impulse point
  ##pcRange = 5 * voicesPerOctave:nScale;
  pcRange = 1:nScale;
  pcRangeIOIs = timeSupport(pcRange, voicesPerOctave);
  printf("Phase congruency from time period %d to %d\n", pcRangeIOIs(1), maxWaveletPeriod);
  pc = phaseCongruency(magnitude(pcRange, :), phase(pcRange, :));

  ## Save for MMA if we have a filename
  if(nargin > 3 && filename != "")
    filename = tilde_expand(filename);
    save([filename ".text"], "signal");
    saveCWT(filename, magnitude, phase, pc);
  endif
endfunction
