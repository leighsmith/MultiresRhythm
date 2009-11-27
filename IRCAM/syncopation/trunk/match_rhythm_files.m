function [ segment_locations, segment_transition_probs ] = match_rhythm_files( query_file, target_file )
%match_rhythm_files Returns the possible locations and probabilities of
%matching segments of the query against the target.
% $Id$

query_rhythm_descr = read_qbt_file(query_file);
target_rhythm_descr = read_qbt_file(target_file);

[ match_locations, segments, segment_locations, segment_location_probs, segment_transition_probs ] = match_rhythm_odf(query_rhythm_descr.wideband_odf', target_rhythm_descr.wideband_odf, query_rhythm_descr.sample_rate);

figure()
plot_correlation_match(query_rhythm_descr.wideband_odf', target_rhythm_descr.wideband_odf', match_locations, segments)

end

