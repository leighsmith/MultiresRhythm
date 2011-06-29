function [windowed_spectrum] = spectrum_of_window(short_term_signal, window)
% spectrum_of_window - From the given windowed signal, return the onset detection function
% and spectrum. 
% Computes a short term spectral analysis via an FFT.
% short_term_signal is assumed dyadic.
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id$
    
    % Compresses the magnitude values to approximate perception of
    % intensity. As used by Klapuri, Eronen & Astola and Grosche & Muller.
    % compression = 1.0; % No compression
    compression = 1000.0; % As reported by Grosche & Muller.

    nyquist = length(short_term_signal) / 2;
    windowed_signal = short_term_signal .* window;
    % We compute the magnitude as abs() which returns the norms of complex values coefficients.
    radian_freq_spectrum = log(1 + compression .* abs(fft(windowed_signal)));
    % fft function returns DC (0th coefficient) centered in vector.
    windowed_spectrum = radian_freq_spectrum(1 : nyquist);
    
    % plot(short_term_signal)
    % figure();
    % plot(windowed_spectrum);
end

