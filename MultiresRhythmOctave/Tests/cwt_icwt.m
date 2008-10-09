### -*- Octave -*-
###
### $Id$
###
### Generate a random signal and convert magnitude and phase back to signal.
### Verifies the invertability of the CWT and therefore it's accuracy.
###

## Must be dyadic (base 2). Otherwise we must pad the signals.
signalLength = 2048; 

periodicity = 16;
index = [1:signalLength];
norm_signal = index / signalLength;
signal = cos(norm_signal * 2 * pi * periodicity);

testReconstruction(signal)

## Reconstruction of a random signal is less successful
testReconstruction(rand(1, signalLength));
