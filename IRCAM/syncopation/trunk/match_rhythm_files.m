function [ segment_locations, segment_transition_probs ] = match_rhythm_files( query_file, target_file )
%match_rhythm_files Returns the possible locations and probabilities of
%matching segments of the query against the target.
% $Id$

query_rhythm_descr = read_analysed_rhythm(query_file, '~/Research/Data/IRCAM-Beat/QueryByTapping/Query/');
target_rhythm_descr = read_analysed_rhythm(target_file, '~/Research/Data/IRCAM-Beat/QueryByTapping/');

[ segment_locations, segment_location_probs, segment_transition_probs ] = match_rhythm_odf(query_rhythm_descr.wideband_odf', target_rhythm_descr.wideband_odf, query_rhythm_descr.sample_rate);

% figure()
% subplot(3,1,1)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(1));
% subplot(3,1,2)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(2));
% subplot(3,1,3)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(3));

end

