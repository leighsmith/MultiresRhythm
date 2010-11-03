function normalised_odf = normalise_odf (odf)
%normalise_odf - Normalise the onset detection function.
% Normalises by dividing by the standard deviation.

    max_of_odf = max(odf)
    min_of_odf = min(odf)
    mean_of_odf = mean(odf)
    stddev_of_odf = std(odf)
    
    % Make the mean zero.
    zero_mean_odf = odf - mean(odf);
    % divide by the stddev, reduces signal to values of roughly +/-3.
    normalised_odf = zero_mean_odf / std(zero_mean_odf);
    % adjust so the minimum value is zero again.
    normalised_odf = normalised_odf - min(normalised_odf);
end


