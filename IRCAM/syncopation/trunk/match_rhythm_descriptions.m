function [ output_args ] = match_rhythm_descriptions ( query_rd, target_rd )
%match_rhythm_descriptions Match query and target rhythm descriptions.
%   Normalises the tempo of the two rhythms ODF's to a common sample rate.

tempo_normalised_query = normalise_by_beats(query_rd.wideband_odf, round(query_rd.beat_times * query_rd.sample_rate), 1/200);
tempo_normalised_target = normalise_by_beats(target_rd.wideband_odf, round(target_rd.beat_times * target_rd.sample_rate), 1/200);

[ match_locations, segments, single_match_measure ] = match_rhythm_odf(tempo_normalised_query, tempo_normalised_target, query_rd.sample_rate);

end

