function [wideband_odf, subband_odfs] = odf_of_signal(audio_signal, original_sample_rate, subband_ranges)
%odf_of_signal - Returns the onset detection function, given an audio signal sampled at original_sample_rate
%
% $Id:$
    
    % The sample rate of the downsampled signal.
    analysis_sample_rate = 11025.0; % In Hertz.
    % The sample rate of the onset detection function.
    % Peeters uses 64, corresponds to 172.2656Hz, interval 5.8mS.
    ODF_sample_rate = 64.0 / analysis_sample_rate;

    % Number of samples processed in each spectral window.
    % Grosche & Muller use 256, corresponding to 23mS.
    window_size = 1024; % Corresponds to 92.78mS. Used by Peeters.
    % Number of samples to overlap each window.
    hop_size = 128; % Corresponds to 11.6mS.

    window_in_seconds = window_size / analysis_sample_rate;
    if (original_sample_rate < analysis_sample_rate)
        fprintf(stderr, 'sample rate %f is below minimum required %f\n', original_sample_rate, analysis_sample_rate);
        return
    end

    % Make mono before resampling to hopefully speed things up a bit.
    mono_audio_signal = sum(audio_signal, 2) / 2.0;
    downsampled_signal = resample(mono_audio_signal, analysis_sample_rate, original_sample_rate);
        
    signal_length = length(downsampled_signal);
    wideband_odf = zeros(1, signal_length); % TODO, needs to be downsampled to ODF
    subband_odfs = zeros(size(subband_ranges, 1), signal_length);  % TODO, needs to be downsampled to ODF

    % Debugging to operate on a single window
    % window_start_sample = 4000; % Fortran base 1 indexing, sigh.
    % windowed_signal = downsampled_signal(window_start_sample : window_start_sample + window_size);
    % windowed_spectrum = spectrum_of_window(windowed_signal);
    spectrum = spectrum_of_signal(downsampled_signal, window_size, hop_size);
    imagesc(spectrum);
    
    % TODO downsample spectrum to the ODF_sample_rate
    % TODO Remove the DC component (0th coefficient) when computing the spectral energy flux.
    
    % Peeters smooths between each successive spectral band using a 3 element moving
    % average. This is needed if we are down sampling the frequency resolution, or if
    % we downsample the time axis, since that will introduce discontinuities in the
    % spectral coefficient axis.
    
    % TODO perhaps a better way is to produce the ODF then downsample it. This uses more
    % data and requires more to be processed, but does not require two filtering
    % processes. We should quantify the factor we save using a downsampled time axis.
    % Particularly if instead we are processing a 10 second window rather than the entire signal.
    
    
    % Determine maximum spectral energy and short circuit if it is below the minimum.
    
    % Peeters sets a further threshold -50dB below maximum energy level.
    
    % Peeters low pass filters over time.
    % Butterworth? center frequency 10Hz, order=5, ODF_sample_rate.
    
    
    % High pass filter using a simple first order differentiator.
    spectrum_derivative = diff(downsampled_spectrum, 1, 2);
    
    % Half wave rectification
    % rectified_spectrum = (spectrum_derivative > 0) .* spectrum_derivative;
    rectified_spectrum = max(spectrum_derivative, 0.0);
    
    wideband_odf = sum(rectified_spectrum);

    % Normalize by removing mean energy levels since energy is +ve only.
    % wideband_odf = abs(wideband_odf - mean(wideband_odf))
    % TODO Could divide by the stddev.
end

