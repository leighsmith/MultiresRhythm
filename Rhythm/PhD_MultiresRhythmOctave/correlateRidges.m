## -*- Octave -*-
##
## $Id$
##
## Compute independent surfaces analysing the magnitude and phase
## which are then combined to form an analytic surface from which we
## then extract a single tactus ridge.
## ahh, parallel machine anyone??
function correlation = correlateRidges(signalDescription, correlationMethod, \
				       magnitude, phase, voicesPerOctave)
  global promptStream;

  ## begin parallelisable bit
  fprintf(promptStream, "Stationary Phase\n");
  statPhase = stationaryPhase(magnitude, phase, voicesPerOctave);

  ## redundant
  ##fprintf(promptStream, "Stationary Phase Ridges\n");
  ##ridgeStatPhase = ridgesStatPhase(magnitude, phase, voicesPerOctave);

  fprintf(promptStream, "Local Phase Congruency\n");
  localPC = localPhaseCongruency(magnitude, phase);

  fprintf(promptStream, "Finding ridge (by scale)\n");

  ## magnitude    = abs(magnitude);
  normalisedMagnitude = normaliseByScale(magnitude);
  ## end parallelisable bit

  ## correlate (by averaging) the energy modulus, stationary phase and
  ## local phase congruency. Since statPhase and local PC are both
  ## conditioned on a minimal magnitude, we reduce false positives.
  correlation = (normalisedMagnitude + statPhase + localPC) / 3;
  ## correlation = normalisedMagnitude;
  ## correlation = statPhase;
  ## correlation = localPC;

  ## show what we got as an intensity plot
  plotCWT("correlation", correlation);

  ## and plot the correlations in profile
##   plotRidgesProfile(signalDescription, 250, \
##  		    normalisedMagnitude, 'Magnitude', \
##  		    localPC, 'Local Phase Congruency', \
##  		    statPhase, 'Stationary Phase', \
##  		    correlatedProfile, 'Correlation',
##  		    ridges, 'Final ridge locations')
  ##  plotRidges(signalDescription, ridges, "Correlated ridges of mag, statPhase, localPC");
  
endfunction

