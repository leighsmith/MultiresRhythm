## -*- Octave -*-
##
## tactus for greensleeves is dotted crochet @ 60 BPM = 300 samples.
## we start clapping on the second beat as the first is an anacrusis.

## "rd" extracts a ridge half the correct foot-tap rate.
## "rd | rlp" extracts a ridge which is the correct foot-tap rate by
## skipping the lower rate due to spurious ridgelettes.

multiresFile("Greensleeves quantized with accent", \
	     "~/Research/Data/NewAnalysedRhythms/greensleeves_accent", 300, 2, "rd | rlp");

