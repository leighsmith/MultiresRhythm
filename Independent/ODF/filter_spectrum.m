function y = filter_spectrum(spectrum)
% Peeters low pass filters over time.
% Butterworth filter center frequency 10Hz, order=5, ODF_sample_rate.
    a = [-1.743077, 0.77188];
    b = [0.009859, 0.009085, 0.009859];
    y = filter(b, a, spectrum, [], 2);
end
