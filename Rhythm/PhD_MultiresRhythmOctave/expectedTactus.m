## -*- Octave -*-
## Determine the expected tactus (where it is known, although for now
## we hardwire it).
## Uses startFrom to compute clap times also.
function [et,claps] = expectedTactus(signal, voicesPerOctave, expectedTactusIOI, startFrom)
  nTime  = length(signal);

  et = zeros(1,nTime);
  et(:) = scaleFromPeriod(expectedTactusIOI,voicesPerOctave);
  beatPositions = find(signal);
  downBeat = beatPositions(startFrom);
  printf("Handclapping from beat %d of original rhythm, sample %d\n", \
  	 startFrom, downBeat);
  clapTimes = downBeat:expectedTactusIOI:nTime;
  nClaps = length(clapTimes);
  claps = zeros(2, nClaps);
  claps(1,:) = clapTimes;
  claps(2,:) = ones(1,nClaps) * 0.5;
endfunction
