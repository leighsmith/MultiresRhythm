## -*- Octave -*-
## Analyse a harmonic (ie. cosinusoidal) signal

signalLength = 2048;
## a nice set of JI ratios 1/1 + octave+8/5 + 2 octaves+6/5
periodicity = 16;
periodicity2 = periodicity * (1 + 8/5);
periodicity3 = periodicity * (4 + 6/5);

index = [1:signalLength];
norm_signal = index / signalLength;
signal = 0.33 .* cos(norm_signal * 2 * pi * periodicity) + \
         0.33 .* cos(norm_signal * 2 * pi * periodicity2) + \
         0.33 .* cos(norm_signal * 2 * pi * periodicity3);

rhythm.signal = signal;
rhythm.description = "harmonic signal";
rhythm.SampleRate = 200;
analysis.beginClapAt = 1;

analysis.expectedTactusIOI = 0;
analysis.correlationMethod = "rd | rsp";
analysis.multiple = 1;

multiresRhythm(rhythm, analysis, "");
