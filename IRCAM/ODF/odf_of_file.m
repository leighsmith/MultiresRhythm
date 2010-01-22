%
% x = odf_of_file(tilde_expand('~/Research/Data/IRCAM-Beat/RWC_excerpts/Audio/rwc_p_81_excerpt.wav'));
function [wideband_odf, subband_odfs] = odf_of_file(filename)
% odf_of_file Compute an onset detection function from the wav file.
% $Id:$
    
subband_ranges = [[60, 100]; [3500, 4000]]; 

[audio_signal, sample_rate, resolution] = wavread(filename);
if (resolution < 16)
    fprintf(stderr, 'dynamic range resolution %d below 16 bits\n', resolution);
end

[wideband_odf, subband_odfs] = odf_of_signal(audio_signal, sample_rate, subband_ranges);

end

