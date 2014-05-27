# -*- Octave -*-
# Analyse an amplitude modulated monochromatic (ie. cosinusoidal)
# signal. The carrier is acoustic 440Hz, the rhythm 4Hz
# 

signalLength = 8192;
acoustic_freq = 440;
rhythm_freq = 4;
index = [1:signalLength];
acoustic = cos((index / signalLength) * 2 * pi * acoustic_freq);
rhythm = cos((index / signalLength) * 2 * pi * rhythm_freq + (pi)) + 1;
plot(rhythm);
signal = acoustic .* rhythm;
# saveaudio(signal) as a .snd file

# cwt_file(rhythm, 8, "~/AnalysedRhythms/am_rhythm", 1024);

cwtToFile(signal, 8, 1024, "~/AnalysedRhythms/am_rhythm");

# maximum wavelet coefficient at 14 scales below the highest,
# 50 scales above 1024 = 13.454

