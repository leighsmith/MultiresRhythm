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
        fprintf('Comparing query %s to target %s\n', query_rhythm_descr.name, target_rhythm_descr.name);
        % Returns the possible locations and probabilities of matching segments of the query against the target.
        [ match_locations, segments, single_match_measure ] = match_rhythm_odf(query_rhythm_descr.wideband_odf, target_rhythm_descr.wideband_odf, query_rhythm_descr.sample_rate);
        dissimilarity(target_index, query_index) = single_match_measure;
    end
end

[best_matches, best_match_indices] = max(dissimilarity);

query_names = cellfun(@rhythm_name, query_descriptions(1 : length(query_descriptions)), 'UniformOutput', false);
matched_target_names = cellfun(@rhythm_name, target_descriptions(best_match_indices), 'UniformOutput', false);

[query_names, matched_target_names]

end


