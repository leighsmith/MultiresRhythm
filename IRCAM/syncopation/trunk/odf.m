function [ odf, odf_sr, odf_subbands ] = odf( wav_filepath )
%odf Compute the onset detection function of the file.
%   Returns both a single dimension onset detection function and a matrix
%   of summed frequency sub-bands.
% $Id$

% MFenergyfft loses the first 30mS of the signal in computing the ODF
seconds_lost = 0.030;

[signal, sample_rate, resolution] = wavread(tilde_expand(wav_filepath));

% low frequencies & high frequencies
subband_range = [[40, 150]; [3000, 4000]];
% alternatively peep into the frequency spectrum at representative frequencies.
% subband_range = [[60, 70]; [2000, 2100]]; 

[odf_fullfreq, marker_times_seconds, odf_subbands_clipped] = MFenergyfft(signal, sample_rate, 0.08, 0.01, 2, subband_range);
odf_sr = 1 / mean(diff(marker_times_seconds));

% Prepend silence to realign the computed ODF to the audio file.
odf = [zeros(round(seconds_lost * odf_sr), 1); odf_fullfreq];
odf_subbands = [zeros(size(odf_subbands_clipped, 1), round(seconds_lost * odf_sr)), odf_subbands_clipped];

end
