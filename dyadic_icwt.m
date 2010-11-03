%%% -*- Octave -*-
%%% $Id$
%%%
%%% Perform the inverse CWT, reconstructing the time domain signal from
%%% the time-frequency domain magnitude and phase components.
%%% Uses the MorletWaveletFourier function to give us a scaled version
%%% of the wavelet and operations are performed in the Fourier domain.
%%% Length of the magnitude/phase (the time axis) is assumed to be a power of 2.
%%%
function signal = dyadic_icwt(magnitude, phase, waveletsPerOct)
  sz = size(magnitude);
  nScale = sz(1)
  nTime = sz(2)

  %% we do everything in transpose, so we transpose the convolved result.
  convolved = zeros(sz(2),sz(1));

  %% Reconstruct coefficients as complex numbers from their magnitude
  %% and phase components.
  complexCoeff = magnitude .* cos(phase) + 1i .* magnitude .* sin(phase);

  %% for the wavelet generation
  period = timeSupport([1:nScale], waveletsPerOct);

  %% to achieve conservation of energy between domains
  %% c_g is chosen according to the wavelet
  c_g = 1.7;
  voiceScaling = c_g .* sqrt(period);

  %% Convert to Fourier domain, fft() operates on each matrix column, so we 
  %% transpose it. We then operate on the result in transpose order,
  %% so scales are columns.
  coeffFFT = fft(complexCoeff.');
  %% loop over each voice, beginning with the highest frequency scale.
  for scaleIndex = 1:nScale
    scaledWavelet = MorletWaveletFourier(nTime, period(scaleIndex));
    %% multiply it with each of the Fourier domain dilated wavelets and
    %% divide by the scale.
    convolved(:,scaleIndex) = ifft(coeffFFT(:,scaleIndex) .* scaledWavelet.') ...
      ./ voiceScaling(scaleIndex);
  end
  %% Sum each columns contributions.
  %% The real portion is the original signal, the imaginary is the
  %% signal 90 degrees out of phase.
  %% This also allows the phase to be extracted.
  signal = sum(convolved.');
endfunction
