## -*- Octave -*-
## Analyse a hyperbolic (ie. cosinusoidal) signal

signalLength = 4096;
index = [1:signalLength];
norm_signal = index / signalLength;
alpha = 100
beta = 50
omegaS = 40

# freq = norm_signal * 2 * pi * omegaS - alpha * exp(1 + beta * norm_signal);
freq = norm_signal * 2 * pi * omegaS + alpha * log(1 + beta * norm_signal);
slowing_signal = cos(freq);

rhythm.signal = slowing_signal;
rhythm.description = "hyperbolic signal";
rhythm.SampleRate = 1/4096;
analysis.beginClapAt = 1;
analysis.expectedTactusIOI = 0;
analysis.correlationMethod = "whatever";
analysis.multiple = 1;

[mag, phase, pc] = multiresRhythm(rhythm, analysis);
# plot(slowing_signal);
