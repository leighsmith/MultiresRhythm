function spectrum_thresholded = threshold_spectral_energy(spectrum)
%threshold_spectral_energy -  
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id$
    
% Determine maximum spectral energy TODO short circuit if it is below the minimum.
    maximum_spectral_energy = max(max(spectrum))
    minimum_spectral_energy = min(min(spectrum))
    
    % Peeters sets a further threshold 50dB below maximum energy level.
    threshold_in_db = 20 * log10(maximum_spectral_energy) - 50
    % Convert back to spectral energy units.
    spectral_energy_threshold = exp(threshold_in_db * log(10) / 20);
    % spectrum_thresholded_db = 20 * log10(max(spectrum, spectral_energy_threshold));
    spectrum_thresholded = max(spectrum, spectral_energy_threshold);
    % minimum_spectrum_energy_db = min(20 * log10(spectrum))
    min(min(spectrum_thresholded))
    max(max(spectrum_thresholded))
end
