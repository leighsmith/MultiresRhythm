function y = filter_spectrum(spectrum)
% Butterworth filter center frequency 10Hz, order=5, ODF_sample_rate.
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id: threshold_spectral_energy.m 5646 2010-05-24 15:54:17Z leighsmi $

    a = [-1.743077, 0.77188];
    b = [0.009859, 0.009085, 0.009859];
    y = filter(b, a, spectrum, [], 2);
end
