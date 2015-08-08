function centroid = spectral_centroid(spectrum)
% spectral_centroid - Given a 2D matrix spectrum, return the centroid.  For each time
% window, calculate the weighted mean of frequency bins (Fourier harmonics), weighted by
% the magnitude of each bin. Assuming bins are numbered from 1 for lowest frequency to
% highest value for highest frequency.
%
% Author: Leigh M. Smith <lsmith@izotope.com>
%
    bin_indices = repmat([1 : size(spectrum, 1)]', 1, size(spectrum, 2));
    total_energy = sum(spectrum);
    centroid = sum(spectrum .* bin_indices) ./ total_energy; 
end


