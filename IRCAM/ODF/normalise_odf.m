function normalised_odf = normalise_odf (odf)
%normalise_odf - Normalise the onset detection function.
% Normalises by dividing by the standard deviation.
    % Make the mean zero.
    zero_mean_odf = odf - mean(odf);
    % divide by the stddev, reduces signal to values of roughly +/-3.
    normalised_odf = zero_mean_odf / std(zero_mean_odf);
    % adjust so the minimum value is zero again.
    normalised_odf = normalised_odf - min(normalised_odf);
end


