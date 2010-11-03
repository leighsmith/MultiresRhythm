## -*- Octave -*-
##
## Plot the phase congruency measure over the top of the signal

function plotPhaseCongruency(comment, signal, pc, highlight, howMuch, eca, eci)
  fprintf(stdout, "Plotting Phase congruency on figure 1\n");
  figure(1);

  xlabel('Time')
  ylabel('Normalised Intensity/Phase Congruency')

  if(nargin < 4)
    title(['Half Phase Congruency and original signal of ', comment]);
    ## plot signal with lines 2 title 'Signal', \
    ## pc.' with lines 3 title 'Phase Congruency';
    plot(signal, "-r;Signal;", pc.', "-b;Phase Congruency;");
  else
    title(['Phase Congruency and original signal of ', comment]);
    peaks = zeros(length(pc), 1);
    peaks(highlight) = howMuch;
    plot(signal, "g;Signal;", pc, "b;Phase Congruency;", peaks, "r^", eca, eci, "m^");
  endif
endfunction





