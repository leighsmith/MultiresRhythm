function [wideband_odf, ODF_sample_rate, subband_odfs, odf_downsampled_signal] = odf_of_signal(audio_signal, original_sample_rate, subband_ranges)
%odf_of_signal - Returns the onset detection function, given an audio signal sampled at original_sample_rate
% Sums the spectrum over given subbands.
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% audio_signal is a PCM audio signal, possibly multichannel (rows).
% original_sample_rate in Hz.
% subband_ranges is an n x 2 column matrix, specifying lower and upper bounds of each
% frequency band in Hz.
%
% TODO perhaps return an object that preserves the state (i.e spectrum etc), otherwise
% just return the spectral centroid alone. Should test on some synthesized broadband
% sounds, i.e a synth sound with known F0.
    
    plotting = true;

    % The sample rate of the downsampled signal.
    analysis_sample_rate = 11025.0; % In Hertz.
    % The hop size determines the number of analysis frames, specifies the window advance as the
    % number of samples to overlap each window.
    % Peeters uses 64 samples corresponding to 172.2656Hz, interval 5.8mS for 11025Hz analysis sample rate.
    % 128 Corresponds to 11.6mS.
    hop_size = 64; 
    % Number of samples processed in each spectral window.
    % Grosche & Muller use 256, corresponding to 23mS.
    window_size = 1024; % Corresponds to 92.78mS. Used by Peeters.

    % The sample rate of the onset detection function.
    ODF_sample_rate = analysis_sample_rate / hop_size;

    window_in_seconds = window_size / analysis_sample_rate;
    if (original_sample_rate < analysis_sample_rate)
        fprintf(stderr, 'sample rate %f is below minimum required %f\n', original_sample_rate, analysis_sample_rate);
        return;
    end

    % Make mono before resampling to hopefully speed things up a bit.
    num_audio_channels = size(audio_signal, 2); % determine from signal.
    mono_audio_signal = sum(audio_signal, 2) / num_audio_channels;
    downsampled_signal = resample(mono_audio_signal, analysis_sample_rate, original_sample_rate);
    % Produce a audio signal downsampled to the ODF sample rate.
    odf_downsampled_signal = resample(mono_audio_signal, round(ODF_sample_rate), original_sample_rate);

    % The spectrum calculation will compute with a hop that produces the ODF_sample_rate.
    spectrum = spectrum_of_signal([zeros(window_size, 1); downsampled_signal], window_size, hop_size);
    
    % Since the energy is calculated over the entire window, the first ODF sample is
    % computed from the window centered over the audio sample half the window length.
    % TODO however, it seems like a full window is consumed, but why?
    % ODF_start_seconds = window_size / (2 * analysis_sample_rate);
    ODF_start_seconds = window_size / analysis_sample_rate;
    ODF_start_padding = ODF_start_seconds * ODF_sample_rate

    % Remove the DC component (0th coefficient) when computing the spectral energy flux.
    spectrum(1,:) = 0;

    % Peeters smooths between each successive spectral band using a 3 element moving
    % average. This is needed if we are down sampling the frequency resolution, or if
    % we downsample the time axis, since that will introduce discontinuities in the
    % spectral coefficient axis. Not clear this is needed.
    
    % filtered_spectrum = filter_spectrum(threshold_spectral_energy(spectrum));
    % filtered_spectrum = spectrum; % postpone the filtering for now.
    filtered_spectrum = filter_spectrum(spectrum);

    % High pass filter using a simple first order differentiator.
    spectrum_derivative = diff(filtered_spectrum, 1, 2);

    % Half wave rectification. Alternatively we could compute the signal energy by squaring.
    % rectified_spectrum = (spectrum_derivative > 0) .* spectrum_derivative;
    rectified_spectrum = max(spectrum_derivative, 0.0);
    size(rectified_spectrum)

    % TODO Could use spectral centroid as a weighting. 
    % Generate a centroid measure, convert to Hz.
    centroid = spectral_centroid(spectrum) .* analysis_sample_rate ./ window_size;

    % TODO In principle the ODF is the magnitude of the total spectral energy at each time
    % window and could be used in calculating a form of spectral centroid.
    
    % TODO Could use a weighting on the summation across bands
    wideband_odf = sum(rectified_spectrum);
    wideband_odf = normalise_odf(wideband_odf);

    plot_region = 1:400;
	
    if nargin > 2
      % create the subband ODFs
      subband_odfs = spectral_subband_odfs(rectified_spectrum, subband_ranges, analysis_sample_rate);
      size(subband_odfs)
      if plotting
	plot_subbands(subband_odfs, plot_region);
      end
    end

    % Plotting
    if plotting
        subplot(3,1,1);
        % On octave, the image is displayed with the y-axis datum 0 at the top left. 
        imagesc(flipud(spectrum(:,plot_region)));
        title(sprintf('Spectrogram of %s', 'signal'));
        subplot(3,1,2);
        plot(wideband_odf(plot_region));
        title(sprintf('Normalised ODF of %s', 'signal'));
        axis([plot_region(1), plot_region(end), 0, 7]);
        subplot(3,1,3);
        plot(centroid(plot_region));
        title(sprintf('Spectral Centroid of %s (Hz)', 'signal'));
        axis([plot_region(1) plot_region(end) min(centroid)-1 max(centroid)+1]);
    end
end

