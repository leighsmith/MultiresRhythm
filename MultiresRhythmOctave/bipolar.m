# -*- Octave -*-

signalLength = 2048;
# multiply the periodicity by two as the 1, -1 pair are samples of
# 0->Pi,Pi->2Pi phase regions respectively.
periodicity = 16 * 2;

bipolarImpulse = zeros(1,signalLength);
bipolarImpulse(1:signalLength/periodicity:signalLength) = real((-1) .^ [1:periodicity]);

cwtToFile(bipolarImpulse, 8, 256, "~/AnalysedRhythms/bipolarPulse_octave"); 
