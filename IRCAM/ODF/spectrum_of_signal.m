function [spectrum] = spectrum_of_signal(signal, window_size, hop_size)
%spectrum_of_signal - Compute the STFT spectrum with overlapping hops & windowing.
% $Id$
    
    signal_length = length(signal);
    spectrum = zeros(window_size / 2, floor((signal_length - window_size) / hop_size) - 1);

    % Precompute the window for the STFT.
    % window = 1.0; % Rectangle
    window = blackman(window_size); % Peeters uses Blackman. Alternative to Hamming window.

    window_start_sample = 1; % Fortran base 1 indexing, sigh.
    window_index = 1;  % Fortran base 1 indexing, sigh.
    % For super long signals, we would need to do it in buffers.
    while (window_start_sample + window_size < signal_length)
        % fprintf('window %d: [%d : %d]\n', window_index, window_start_sample, window_start_sample + window_size - 1);
        windowed_signal = signal(window_start_sample : window_start_sample + window_size - 1);
        spectrum(:, window_index) = spectrum_of_window(windowed_signal, window);
        window_start_sample = window_start_sample + hop_size;
        window_index = window_index + 1;
    end
end
