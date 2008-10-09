### -*- Octave -*-
### $Id$

number_of_samples = 4096;

norm_signal = 0 : 1 / number_of_samples : 1;

## 4 -> 16
periodicity = (norm_signal * 12) + 4;

rising_signal = cos(norm_signal * 2 * pi .* periodicity);

rhythm.signal = rising_signal;
rhythm.description = "rising monochromatic sinusoid";
rhythm.SampleRate = number_of_samples;

analysis.correlationMethod = "rd";
analysis.expectedTactusIOI = 0;
analysis.beginClapAt = 1;

[magr, phaser, pcr] = multiresRhythm(rhythm, analysis, "");
