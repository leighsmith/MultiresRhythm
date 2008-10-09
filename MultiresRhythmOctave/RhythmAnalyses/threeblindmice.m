## -*- Octave -*-
##
## Three Blind Mice
## From a suggestion by Todd, O'Boyle and Lee
## Do the analysis on the unquantised version
#[m, p, pc_u] = multiresFile("Three Blind Mice Performed", \
			    #"~/AnalysedRhythms/desain-unquantized", \
			    #0, 1, "rd | rlp | rsp");

## Now do the analysis on the quantised version
## the expected tactus corresponds to a minim @ 140BPM @ 200Hz = 171 samples
## if it corresponded to a crochet @ 140BPM @ 200Hz = 86 samples interval
## "rd"  - finds a tactus too low frequency
## "rd | rlp" - couldn't find tactus
## "rd | rsp" - couldn't find tactus, but finds most pulse rates
## "rd | rsp | rlp" - extracts a curvy tactus, too fast
## "rsp | rlp" - couldn't find tactus
## "rd | (rsp & rlp)"
[m, p, pc_q] = multiresFile("Three Blind Mice Quantized", \
			    "~/AnalysedRhythms/ThreeBlindMice", \
			    86, 4, "rd | rlp");

#fprintf(stderr, "Press a key to plot comparative Phase Congruency plot\n");
#pause
#title('Comparison of Phase Congruency of Desain Connectionist Unquantized and Quantized examples');
#xlabel('Time')
#ylabel('Phase Congruency')
#plot(pc_u, "b", pc_q, "g");
#pause
#title('Phase Congruency of Three Blind Mice Quantized example');
#plot(pc_q, "b")
#pause
#title('Phase Congruency of Three Blind Mice Unquantized example');
#plot(pc_u, "g")



