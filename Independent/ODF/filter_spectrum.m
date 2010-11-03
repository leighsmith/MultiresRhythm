function y = filter_spectrum(spectrum)
    a = [-1.743077, 0.77188];
    b = [0.009859, 0.009085, 0.009859];
    y = filter(b, a, spectrum, [], 2);
end
