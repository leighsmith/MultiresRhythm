### -*- Octave -*-
###
### Function to compute times to clap and how hard from the extracted tactus.

function claps = \
	handClapToTactusSample(signal, tactus, multiple, voicesPerOctave, nScale, startFrom)
  nTime = length(signal);

  ## Make an oversized array so we can resize when we know how many beats
  ## there are.
  clapTimes = zeros(1, nTime);
  ## get the first beat from the original rhythm so we get the phase right.
  clapNum = 1;			# index to our clapping beats
  beatPositions = find(signal);
  downBeat = beatPositions(startFrom);

  printf("Handclapping from beat %d of original rhythm, sample %d\n", \
	 startFrom, downBeat);

  ## compute each successive downBeat by iteratively determining the IOI
  ## from the tactus.
  while(downBeat < nTime)
    clapTimes(clapNum++) = downBeat;
    newScale = tactus(1, downBeat);
    newPhase = tactus(2, downBeat);
    ## Ok, this is the crux of the matter, if this is wrong, everything will be.
    ## We verify the scale according to a sinusoid and pulse of known frequency.
    ## at least the computation is reused in several functions.
    newIOI = timeSupport(nScale - newScale, voicesPerOctave);

    ## Here we determine the possible uncertainty error from the dilation
    ## resolution. Due to the logarithmic dilation, the +/- differences
    ## are different (but similar in magnitude).
    clapErrUnder = timeSupport(nScale - newScale - 1, voicesPerOctave);
    clapErrOver  = timeSupport(nScale - newScale + 1, voicesPerOctave);
    printf("Next IOI %.2f, inaccuracy of %.2f, between (%.2f,%.2f)\n", \
	   newIOI, clapErrOver - clapErrUnder, clapErrUnder, clapErrOver);

    ## TODO Check if a original beat falls within the clapPosition
    ## how well the claps fall within the
    ## uncertainty period surrounding the original beat signal.

    ## TODO yeah but from what?
    ## phaseOffset = (newPhase / (2 .* pi)) + 0.5

    downBeat = downBeat + floor(newIOI / multiple);
  endwhile

  claps = zeros(2, clapNum-1);
  claps(1,:) = clapTimes(1:clapNum-1);	# resize the array to the number of beats
  ## use constant intensity claps but weight the amplitude for when we mix.
  claps(2,:) = ones(size(clapAt)) * 0.6; 
endfunction
