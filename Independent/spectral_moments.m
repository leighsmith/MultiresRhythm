[audio_signal, sample_rate, resolution] = wavread(tilde_expand(filename));
subband_ranges = [[100, 200]; [201, 400]; [401, 800]; [801, 1600]; [1601, 3200]; [3201, 5511]];

analysis_sample_rate = 11025.0; % In Hertz.
hop_size = 64; 
window_size = 1024; % Corresponds to 92.78mS. Used by Peeters.

% The sample rate of the onset detection function.
ODF_sample_rate = analysis_sample_rate / hop_size;

window_in_seconds = window_size / analysis_sample_rate;
num_audio_channels = size(audio_signal, 2); % determine from signal.
mono_audio_signal = sum(audio_signal, 2) / num_audio_channels;
downsampled_signal = resample(mono_audio_signal, analysis_sample_rate, original_sample_rate);
downsampled_signal = resample(mono_audio_signal, analysis_sample_rate, sample_rate);
spectrum = spectrum_of_signal([zeros(window_size, 1); downsampled_signal], window_size, hop_size);
ODF_start_seconds = window_size / analysis_sample_rate;
ODF_start_padding = ODF_start_seconds * ODF_sample_rate;

% Remove the DC component (0th coefficient) when computing the spectral energy flux.
spectrum(1,:) = 0;
centroid = spectral_centroid(spectrum);
% The centroid units are in frequency bin indices.

window_to_plot = 12;
spectrumSlice = spectrum(:,window_to_plot);

figure

centroidSpectralBin = round(centroid(window_to_plot));
centroidLocation = zeros(1:size(spectrumSlice));
centroidLocation(centroidSpectralBin) = spectrumSlice(centroidSpectralBin);
# stem(spectrumSlice, '-', centroidLocation , 'b');
plot(spectrumSlice, '-', centroidSpectralBin, spectrumSlice(centroidSpectralBin), '*');
title("Spectral Centroid");
xlabel("Spectral Bin");
ylabel("Power");
