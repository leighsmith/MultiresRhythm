### -*- Octave -*-
###
### $Id$
###
### Analyse a 'monochromatic' (ie. cosinusoidal) signal
###
signalLength = 2048;
periodicity = 16;
index = [1:signalLength];
norm_signal = index / signalLength;
signal = cos(norm_signal * 2 * pi * periodicity);
				# plot(signal)

rhythm.signal = signal;
rhythm.SampleRate = 200; # just use the default.
rhythm.description = "monochromatic sinusoid";
analysis.beginClapAt = 1;
analysis.expectedTactusIOI = 0;
analysis.correlationMethod = "rd | rlp | rsp";
analysis.multiple = 1;

[mag, phase, pc] = multiresRhythm(rhythm, analysis, "");


