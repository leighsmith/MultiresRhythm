### -*- Octave -*-
### $Id: rising.m 4699 2006-02-08 23:06:08Z leigh $
### Analyse a harmonic (ie. cosinusoidal) signal

signalLength = 2048;
norm_signal = 0 : 1 / signalLength : 1;

## 4 -> 20
periodicity = (norm_signal .* 16) + 4;
## a nice set of JI ratios 1/1 + octave+8/5 + 2 octaves+6/5
periodicity2 = periodicity .* (1 + 8/5);
periodicity3 = periodicity .* (4 + 6/5);

signal = 0.33 .* cos(norm_signal .* 2 * pi .* periodicity) + \
         0.33 .* cos(norm_signal .* 2 * pi .* periodicity2) + \
         0.33 .* cos(norm_signal .* 2 * pi .* periodicity3);

rhythm.signal = signal;
rhythm.description = "rising harmonic signal";
rhythm.SampleRate = 200;

analysis.beginClapAt = 1;
analysis.expectedTactusIOI = 0;
analysis.correlationMethod = "rd | rsp";
analysis.multiple = 1;

multiresRhythm(rhythm, analysis, "");

## ridgeTests(rhythm);
