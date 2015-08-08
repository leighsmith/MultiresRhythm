# -*- Octave -*-
# Analyse a 'monochromatic ' (ie. cosinusoidal) signal

signalLength = 2048;
periodicity = 16;
index = [1:signalLength];
rising_signal = cos((index / signalLength) * 2 * pi * 16 .* ((index / signalLength) + 1));
accelerating_rhythm = rising_signal > 0.99;

cwtToFile(accelerating_rhythm, 8, 512, "~/AnalysedRhythms/accelerando");
