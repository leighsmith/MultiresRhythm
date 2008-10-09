## -*- Octave -*-
## Analyse a frequency modulated sinusoid signal
signalLength = 2048;
carrierFreq = 16;
modFreq = 2.1;
index = [1:signalLength];
norm_signal = index / signalLength;

mod = 3.0 .* cos(norm_signal * 2 * pi * modFreq);
plot(mod)

fm_signal = cos(2 * pi * carrierFreq * norm_signal + mod);
plot(fm_signal)

rhythm.signal = fm_signal;
rhythm.description = "FM signal";
analysis.beginClapAt = 1;
analysis.expectedTactusIOI = 0;
analysis.correlationMethod = "rd | rlp | rsp";
analysis.multiple = 1;

[magf, phasef, pcf] = multiresRhythm(rhythm, analysis, "");

## [mag, phase] = dyadic_cwt(fm_signal, 8, signalLength ./ 4);
## plotCWT(rhythm.description, mag, phase);
