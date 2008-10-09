# -*- Octave -*-
# Analyse accelerating rhythm
# this is obselete as it can't be guarenteed to give single impulses.

signalLength = 2048;
periodicity = 16;
index = [1:signalLength];
rising_signal = cos((index / signalLength) * 2 * pi * 16 .* ((index / signalLength) + 1));
accelerating_rhythm = rising_signal > 0.99;

cwt_file(accelerating_rhythm, "~/AnalysedRhythms/accelerando", 8, 512);
