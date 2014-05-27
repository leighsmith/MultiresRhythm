%% Continuous Wavelet transform in Fourier Domain
%% hacked up in Octave (matlab-alike)
%%
%% By Leigh Smith, 6/2/1998
%%
%% Uses the MorletWaveletFourier function to give us a scaled version
%% of the wavelet.
%% Length of the inputData is assumed to be a power of 2 (dyadic).
%% Does the convolution by multiplying in the Fourier domain.
%% The maximumTimePeriod can really only be about 1/4 the inputData to
%% still be representing the wavelets in the freq and time domains
%% meaningfully.
%%
%% -*- Octave -*-
function [magnitude, phase] = dyadic_cwt(inputData, waveletsPerOct, maximumTimePeriod)
  nScale = scaleFromPeriod(maximumTimePeriod, waveletsPerOct)
  nTime = length(inputData);

  printf('Maximum Time Period analysed = %d, dyadic length = %d\n', ...
	 maximumTimePeriod, nTime);

  %% nTime is assumed to be a power of 2.
  inputDataFFT = fft(inputData);

  magnitude = zeros(nScale, nTime);
  phase = zeros(nScale, nTime);

  %% Widening period (scale) => increasing time dilation,
  %% i.e. lower frequency, narrowing bandwidth in the Fourier domain.
  period = timeSupport([1:nScale], waveletsPerOct);

  %% Energy Normalisation term to keep energy of the scaled wavelets the
  %% same as the mother wavelet. Scale the contribution of each voice by
  %% its index as the coefficient has a representation 1/sqrt(a) *
  %% W(b,a).
  voiceScaling = sqrt(period);
  %% voiceScaling = voiceScaling .* gauss(nScale); plot(voiceScaling,"4");

  %% loop over each voice, beginning with the highest frequency scale.
  for scaleIndex = 1:nScale
    %% Construct the scaled wavelet in the frequency domain.
    %% it will be the same length as the inputData, but mostly zero
    %% outside the Gaussian shape.
    scaledWavelet = MorletWaveletFourier(nTime, period(scaleIndex));
    %% scaledWavelet = SombreroWaveletFourier(nTime, period(scaleIndex));

    voiceResponse = ifft(inputDataFFT .* scaledWavelet .* voiceScaling(scaleIndex));

    %% Produce Magnitude/Phase components from Real/Imag

    %% magAtScale = abs(voiceResponse) ./ voiceScaling(scaleIndex); 
    %% magAtScale = abs((voiceResponse .^ 2) ./ period(scaleIndex)); 
    magAtScale = abs(voiceResponse); 
    magnitude(scaleIndex, :) = magAtScale;

    %% If the magnitude value is low, the phase will be ill-conditioned,
    %% which we will plot different to significant phase behaviour.
    %% atan2 used by arg will return -pi to pi.
    phase(scaleIndex, :) = arg(voiceResponse);
  end
  printf('Finished CWT, last time period = %d\n', period(nScale));
endfunction
