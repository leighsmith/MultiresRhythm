function y = filter_spectrum(spectrum)
% Butterworth filter center frequency 10Hz, order=5, ODF_sample_rate.
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id$

    a = [-1.743077, 0.77188];
    b = [0.009859, 0.009085, 0.009859];
    y = filter(b, a, spectrum, [], 2);
end
