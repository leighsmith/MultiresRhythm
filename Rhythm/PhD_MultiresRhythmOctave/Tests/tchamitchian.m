## -*- Octave -*-
## Now do a version of Tchamitchian and Torresani's pp133 Fig 2.

signalLength = 1500;
index = [1:signalLength];
reduce = 1:4:signalLength;
alpha = 2.5
omegaS = 250

norm_signal = index / signalLength;
triangularAmp = zeros(size(index));
triangularAmp(1:1000) = (1:1000) / 1000;
triangularAmp(1001:1500) = 1 - ((1:500) / 500);
plot(triangularAmp)

freq = ((2 * pi * omegaS) / alpha) * log(1 + alpha * norm_signal);
signal = triangularAmp.' .* cos(freq).';
signalDescription = "Tchamitchian and Torresani's enveloped sinusoid";
title(signalDescription);
plot(freq)
pause
plot(signal)

[mag, phase, pc] = cwtToFile(signal, 16);

##gset hidden3d;
##gsplot(mag);
##pause

##  phase = cleanPhase(mag, phase);
rd = ridgesDilation(mag);

stationaryPhase = ridgesStatPhase(mag, phase, 16);
plotRidges(signalDescription, rd, "dilation ridges", stationaryPhase, "stationary phase");
plotCWTAndRidge(mag, phase, stationaryPhase);
