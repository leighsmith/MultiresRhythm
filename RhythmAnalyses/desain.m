## -*- Octave -*-
## $Id$
##
## Desain and Honing's Quantization problem example.
## Do the analysis on the unquantised version
[m, p, pc_u] = multiresFile("Desain Connectionist example unquantized", \
			    "~/Research/Data/AnalysedRhythms/desain-unquantized", \
			    0, 2, "rd | rlp | rsp");

## Now do the analysis on the quantised version
## the expected tactus corresponds to a crochet @ 200Hz = 200 samples
## "rd"  - couldn't find tactus
## "rd | rlp" - couldn't find tactus
## "rd | rsp" - couldn't find tactus, but finds most pulse rates
## "rd | rsp | rlp" - extracts a curvy tactus, too fast
## "rsp | rlp" - couldn't find tactus
## "rd | (rsp & rlp)"
[m, p, pc_q] = multiresFile("Desain Connectionist example Quantized", \
			    "~/Research/Data/AnalysedRhythms/desain-quantized", \
			    200, 1, "rd | rsp | rlp");

fprintf(stderr, "Press a key to plot comparative Phase Congruency plot\n");
pause
title('Comparison of Phase Congruency of Desain Connectionist Unquantized and Quantized examples');
xlabel('Time')
ylabel('Phase Congruency')
plot(pc_u, "b", pc_q, "g");
pause
title('Phase Congruency of Desain Connectionist Unquantized example');
plot(pc_u, "b")
pause
title('Phase Congruency of Desain Connectionist Quantized example');
plot(pc_q, "g")



