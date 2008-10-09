## -*- Octave -*-
##
## Desain and Honing's Web rhythm example.
##
## Do the analysis on the quantised version
## tactus IOI in samples, for honing given crochet @ 100BPM = tactus = 120
##"rd | rlp | rsp" - fails
## "rd" - extracts a low ridge
## "rd | rsp" - extracts a low ridge with better tactus ridges
multiresFile("Desain and Honing's Web Rhythm (Quantised)", \
	"~/AnalysedRhythms/honing-unaccented", 120, 1, "rd | rsp");

## Now do the analysis on the rubato version
##multiresFile("Desain and Honing's Web Rhythm with rubato", 
##	     "~/AnalysedRhythms/honing-rubato-unaccented", 0, 1, "rd | rsp");

## Now do the test of the rubato applied to a isochronous pulse.
##multiresFile("Rubato applied to an isochronous pulse", \
##	     "~/AnalysedRhythms/pulse-rubato-test", 0, 1, "rd");
