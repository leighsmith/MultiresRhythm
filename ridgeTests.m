### -*- Octave -*-
### This routine just plots each ridge computed individually on the same
### graph for comparison.
function ridgesFound = ridgeTests(testSignal)
  figure(1);
  title(testSignal.description);
  plot(testSignal.signal);

  [mag, phase, pc] = cwtToFile(testSignal.signal, 16);

  ##  phase = cleanPhase(mag, phase);
  rd = ridgesDilation(mag);
  ##wrapPhase = ridgesPhaseWrap(mag,phase);
  ##plotRidges("dilation ridges and phase wrap", rd, wrapPhase);

  ## plotCWTAndRidge(mag(:,reduce), phase(:,reduce), wrapPhase(:,reduce));

  ## pause

  ## statPhase = stationaryPhase(mag, phase, 16);

  stationaryPhaseRidges = ridgesStatPhase(mag, phase, 16);
  ## strata_plot(rd(62:66,3050:3325), statPhase(62:66,3050:3325));

  localPC = ridgesLocalPC(mag, phase, 16);

  ##plotRidges(signalDescription, stationaryPhaseRidges, "stationary phase");
  plotRidges(testSignal.description, rd, "dilation ridges", stationaryPhaseRidges, "stationary phase");
  ##plotRidges(signalDescription, stationaryPhase, "stationary phase", \
  ##	   rd, "dilation ridges", localPC, "local PC");
  ## plotCWTAndRidge(mag(:,reduce), phase(:,reduce), stationaryPhase(:,reduce));
  
  ## and plot the correlations
  ## plotRidgesProfile('Burp signal', 200, \
  ## 		  mag, 'Magnitude', \
  ## 		  stationaryPhase, 'Stationary Phase',
  ##  		  localPC, 'Local Phase Congruency')
endfunction
