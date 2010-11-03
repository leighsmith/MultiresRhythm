# -*- Octave -*-
#
## Calculate a dimensionless measure of local energy at each time
## point given matrices containing magnitude and phase components
## of the Wavelet coefficients. 
## Phase is assumed to be -pi to pi, or -5 if the magnitude is below 
## threshold.
## Hacked up in Octave (matlab-alike)
## By Leigh Smith, 4/3/1998

function pc = phaseCongruency(magnitude, phase)
  realBit = magnitude .* cos(phase);
  imagBit = magnitude .* sin(phase);
  localEnergy = sqrt(sum(realBit) .^ 2 + sum(imagBit) .^ 2);
  localMagnitude = sum(magnitude);
  pc = localEnergy ./ localMagnitude;
endfunction
