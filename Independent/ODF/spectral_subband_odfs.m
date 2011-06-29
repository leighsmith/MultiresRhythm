function subband_odfs = spectral_subband_odfs(rectified_spectrum, subband_ranges, analysis_sample_rate)
%spectral_subband_odfs - Calculate the onset detection functions on the spectral subbands.
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id$
    
spectral_bandwidth = size(rectified_spectrum, 1); % Num of +ve frequency spectral coefficients.
odf_length = size(rectified_spectrum, 2); % The length of the onset detection function in hop_size frames.
subband_odfs = zeros(size(subband_ranges, 1), odf_length);

% Convert subband ranges from frequencies to spectral coefficients & therefore rows.
% We use twice the spectral bandwidth since it is only the number of +ve frequencies.
subband_indices = round((subband_ranges / analysis_sample_rate) * 2 * spectral_bandwidth);

for subband_index = 1 : size(subband_indices, 1)
    band_index_range = subband_indices(subband_index, 1) : subband_indices(subband_index, 2);
    subband_odfs(subband_index, :) = sum(rectified_spectrum(band_index_range, :), 1);
end

end
