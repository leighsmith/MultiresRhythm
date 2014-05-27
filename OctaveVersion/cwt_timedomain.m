# -*- Octave -*-
#
## Continuous Wavelet transform convolving in the time domain
## hacked up in Octave (matlab-alike)
## By Leigh Smith, 6/2/1998
## uses the MorletWavelet function to give us a scaled and translated version
## of the wavelet.

function [magnitude, phase] = cwt_timedomain(inputData, waveletsPerOct, maximumPeriod)

  nFreq = waveletsPerOct * (log2(maximumPeriod) - 1.0) + 1.0

  # Should be a power of 2.
  nTime = length(inputData)

  magnitude = zeros(nFreq, nTime);
  phase = zeros(nFreq, nTime);

  # loop over each voice
  for freqIndex = 1:nFreq
    period = 2.0 ^ (freqIndex / waveletsPerOct) + 1.0;

    # hardwire the k parameter=2.
    scaled_wavelet = MorletWavelet(period, 2.0);

#    plot(real(scaled_wavelet),"1",imag(scaled_wavelet),"2")

    retain = translate_convolve(inputData, scaled_wavelet);

#    printf("temp length = %d\n", length(temp));
#    printf("wavelet length = %d\n", length(scaled_wavelet));

    # Produce Magnitude/Phase components from Real/Imag

    # make negative to have the greyscale images display dark for high
    # modulus values
    magnitude(freqIndex, :) = -abs(retain);

    # atan2 will return -pi to pi, which we can think of as 0 - 2 pi
    phase(freqIndex, :) = atan2(imag(retain), real(retain));

  endfor

  # nrg = LocalEnergy(temp);
  # magsum = SumMagnitudes(temp);

endfunction
