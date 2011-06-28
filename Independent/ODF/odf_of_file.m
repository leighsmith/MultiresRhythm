function [wideband_odf, odf_sr, subband_odfs] = odf_of_file(filename, subband_ranges)
% odf_of_file Compute an onset detection function from the wav file.
% [wodf, odf_sr, subband_odfs] = odf_of_file(tilde_expand('~/Research/Data/IRCAM-Beat/RWC_excerpts/Audio/rwc_p_81_excerpt.wav'));
% $Id$
%

[audio_signal, sample_rate, resolution] = wavread(tilde_expand(filename));

if (resolution < 16)
    fprintf(stderr, 'dynamic range resolution %d below 16 bits\n', resolution);
end

if(nargin < 2)
    % low frequencies & high frequencies
    % subband_range = [[40, 150]; [3000, 4000]];
    % alternatively peep into the frequency spectrum at representative frequencies.
    % subband_ranges = [[60, 100]; [3500, 4000]]; 
    % TODO split across sample_rate / 2
    subband_ranges = [[100, 200]; [201, 400]; [401, 800]; [801, 1600]; [1601, 3200]; [3201, 5511]];
end

fprintf('%s %d samples @ %f Hz, each %d bits\n', filename, length(audio_signal), sample_rate, resolution);
[wideband_odf, odf_sr, subband_odfs] = odf_of_signal(audio_signal, sample_rate, subband_ranges);

end

