## -*- Octave -*-
##

## rlp doesn't much impact, probably because the magnitude is too
## low. TODO try removing the threshold condition on ridgesLocalPC.
[mag, phase, pc, tactus, claps] = \
    multiresFile("Rubato dependent agogic deviation (partial correction)", \
		 "~/AnalysedRhythms/test-agogic", 0, 5, "rd | rlp | rsp");
