### -*- Octave -*-
### $Id$
###
### Perform all the analyses on the given signal.
###
### rhythm is a structure (or object in OOP terms) with fields
### describing characteristics of the input data:
### description: Textual comment.
### signal: a 1xN vector.
### SampleRate: giving absolute timing.
###
### analysis is a structure/object with fields describing the analysis
### to be performed on the the rhythm:
### correlationMethod: holding the means of ridge correlation.
### expectedTactusIOI:
### beginClapAt:
### TODO voicesPerOctave
function [mag, phase, pc, tactus, claps] = multiresRhythm(rhythm, analysis, filename)
  ## send all our prompts to an appropriate stream so that they sequence
  ## with informational output correctly. This seems octave version dependent.
  global promptStream = stdout;
  page_screen_output = 0;

  ## Default plots to figure 0.
  figure(0);

  ## Plot the signal
  plot(rhythm.signal, sprintf("-r;%s;", rhythm.description));

  voicesPerOctave = 16;

  if(nargin < 3)
    [mag, phase, pc] = cwtToFile(rhythm.signal, voicesPerOctave);
  else
    [mag, phase, pc] = cwtToFile(rhythm.signal, voicesPerOctave, -1, filename, 1);
  endif

  plotPhaseCongruency(rhythm.description, rhythm.signal, pc);

  sz     = size(mag);
  nScale = sz(1);
  nTime  = sz(2);

  ## Scale index 1 is the highest frequency (smallest dilation) scale.
  salientScale = preferredTempo(mag, phase, voicesPerOctave, rhythm.SampleRate);
  printf("preferred tempo scale = %d\n", salientScale);

  ## Correlate various ridges to produce a robust version.
  ## Ok, probably not only one ridge results....
  oneTrueRidge = determineRidges(correlateRidges(rhythm.description, analysis.correlationMethod, mag, phase, voicesPerOctave));

  ## Write out the ridges to a file for the Lisp version of the code to
  ## select a tactus.
  save([filename, ".ridges"], "oneTrueRidge");

  ## select out the tactus (array of scaleIndexes) from all ridge candidates.
  tactus = extractTactus(oneTrueRidge);

  ## We set any ill-conditioned phase (negligible magnitude) to zero here
  ## after we take the phase derivative (in ridgesStatPhase) as it would
  ## create false changes.
  phase = cleanPhase(mag, phase);

  ## kludge
  ## tactus = straightenTactus(tactus);

  ## tactus? = detrend(oneTrueRidge);
  ## tactus = extractTactusWalkingHills(oneTrueRidge);

  ## use the tactus to deduce handclap (i.e foottapping) times
  [claps, footTapOscillation] = handClapToTactusPhase(rhythm.signal.', mag, phase, tactus, voicesPerOctave, analysis.beginClapAt);

  ## display the IOI's of the claps.
  ## diff(claps(1,:));

  if(analysis.expectedTactusIOI != 0)
    ## determine where the tactus should be
    [expectedTactus, expectedClaps] = expectedTactus(rhythm.signal, voicesPerOctave, analysis.expectedTactusIOI, analysis.beginClapAt);
    printf("expected tactus scale = %d\n", expectedTactus(1,1));

    ## plotRidges(rhythm.description, rd, rsp, rlp, tactus, expectedTactus);
    ## plotTactus(rhythm.description, oneTrueRidge, tactus, expectedTactus);
    plotRidgesAndTactus(rhythm.description, oneTrueRidge, tactus, 1, expectedTactus);
    ## use the ideal expected tactus to determine expected handclap times
    [expectedClapsComputed, expectedTapPhase] = \
    handClapToTactusPhase(rhythm.signal.', mag, phase, expectedTactus, \
		     voicesPerOctave, analysis.beginClapAt);
    plotClaps("comparing foot-tap to notated expected foot-tap and foot-tap oscillation", 
	      rhythm.description, rhythm.signal, claps, expectedClaps, footTapOscillation);
    plotClaps("comparing foot-tap to notated expected foot-tap", 
	      rhythm.description, rhythm.signal, claps, expectedClaps);
    plotClaps("comparing computed expected to notated expected and expected tap phase", 
	      rhythm.description, rhythm.signal, expectedClapsComputed, expectedClaps, expectedTapPhase);
    plotClaps("comparing computed expected to notated expected", 
	      rhythm.description, rhythm.signal, expectedClapsComputed, expectedClaps);

    ## determine the IOI's of the expected values.
    ecc_ioi = diff(expectedClapsComputed(1,:))
    ec_ioi = diff(expectedClaps(1,:))
    ## ec_ioi - ecc_ioi

  else
    ## plotRidges(rhythm.description, rd, "ridges dilation", rsp,
    ## "stationary phase", rlp, "local PC", tactus, "tactus");
    fprintf(stdout, "Plotting tactus and claps, not expected rhythm\n");
    ## plotTactus(rhythm.description, oneTrueRidge, tactus);
    plotRidgesAndTactus(rhythm.description, oneTrueRidge, tactus, 1);
    plotClaps("ridges and computed tactus", rhythm.description, rhythm.signal, claps, claps, footTapOscillation);
  endif
  ## plotClaps("original rhythm and final foot-taps", rhythm.description, rhythm.signal, claps);

endfunction
