## -*- Octave -*-

## This is our standby if promptStream is undefined.
global promptStream;
promptStream = stdout;

signalLength = 4096;
index = [1:signalLength];
reduce = 1:4:signalLength;
norm_signal = index / signalLength;
alpha = 100
beta = 50
omegaS = 40

## freq = norm_signal * 2 * pi * omegaS - alpha * exp(1 + beta * norm_signal);
freq = norm_signal * 2 * pi * omegaS + alpha * log(1 + beta * norm_signal);
signal.signal = cos(freq);
signal.description = 'logarithmically decreasing frequency sinusoid signal';

figure(0);
title(signal.description);
plot(freq);

ridgeTests(signal);
