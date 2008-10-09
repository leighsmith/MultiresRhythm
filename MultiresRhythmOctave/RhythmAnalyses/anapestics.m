## -*- Octave -*-
##
## Todd's anapestic rhythm example
## Do the analysis on the isochronous version
##multiresFile("Todd's isochronous anapestic rhythm", \
##	     "~/AnalysedRhythms/todd-anapest", 0, 1, "rd");


## The artifactual acceleration at the start perturbs the phase, so we
## start clapping from the third anapest group (the seventh beat).
[mag, phase, pc, tactus, claps] = \
    multiresFile("Anapestic rhythm with rubato", \
		 "~/Research/Data/NewAnalysedRhythms/anapest-with-rubato", 0, 7, "rd | rlp");

anapestics = loadRhythm("~/Research/Data/NewAnalysedRhythms/anapest-with-rubato");
downbeat_interval = sum(reshape(diff(find(anapestics)), 3, 21))
foottap_interval = [claps(1,1) diff(claps(1,:))]
error = downbeat_interval - foottap_interval
error_msec = (error ./ 200) .* 1000
