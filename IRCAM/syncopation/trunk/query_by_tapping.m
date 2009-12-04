function [ dissimilarity ] = query_by_tapping( corpus_directory )
%query_by_tapping Summary of this function goes here
% $Id$

corpus_directory = '~/Research/Data/IRCAM-Beat/QueryByTapping';

fprintf('Reading targets\n');
target_descriptions = all_target_descriptions(corpus_directory);
fprintf('Reading queries\n');
query_descriptions = all_query_descriptions(corpus_directory);
dissimilarity = zeros(length(target_descriptions), length(query_descriptions));

for target_index = 1 : length(target_descriptions)
    target_rhythm_descr = target_descriptions{target_index};
    for query_index = 1 : length(query_descriptions)
        query_rhythm_descr = query_descriptions{query_index};
        % Returns the possible locations and probabilities of matching segments of the query against the target.
        [ match_locations, segments, single_match_measure ] = match_rhythm_odf(query_rhythm_descr.wideband_odf, target_rhythm_descr.wideband_odf, query_rhythm_descr.sample_rate);
        dissimilarity(target_index, query_index) = single_match_measure;
    end
end

end


