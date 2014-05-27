### -*- Octave -*-
###
### $Id: cwt_icwt.m 4685 2006-01-24 23:19:05Z leigh $
###
### Verifies the invertability of the CWT and therefore it's accuracy.
###
function testReconstruction(signal)
  voicesPerOctave = 8;

  plot(signal)

  [mag, phase] = dyadic_cwt(signal, voicesPerOctave, length(signal) ./ 4);

  plotCWT("random signal", mag, phase);

  reconstructedSignal = dyadic_icwt(mag, phase, voicesPerOctave);

  ## The real component of the reconstructed signal is the original
  ## signal, not the magnitude, since it's a Hilbert transform.
  ## magnitude = abs(reconstructedSignal);
  ## The phase is derived from the the real and imaginary
  ## components.
  ## phase = arg(reconstructedSignal);
  
  plot(signal, "r", real(reconstructedSignal), "b")

  printf("maximum difference between signal and reconstruction %f\n", max(signal - real(reconstructedSignal)));
endfunction
