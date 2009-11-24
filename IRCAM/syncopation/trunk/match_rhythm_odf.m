function [ sorted_locations, sorted_location_probs, segment_transition_probs ] = match_rhythm_odf( query_odf, target_odf, sample_rate )
%match_rhythm_odf Returns the possible locations and probabilities of
% matching segments of the query against the target.
% $Id$

segments = segment_odf(query_odf, sample_rate);

number_of_segments = size(segments,1);

[sorted_locations, sorted_location_probs] = match_segments(query_odf, target_odf, segments);

number_of_matches = size(sorted_locations, 1);

segment_transition_probs = segment_transition_probabilities(segments, sorted_locations);

imagesc(segment_transition_probs(:,:,2));
segment_transition_probs(:,:,2)

% imagesc(segment_location_probs(sorted_indices));
figure();
imagesc(sorted_location_probs);

% Could weight the initial probabilities toward earlier rhythmic structures.
% Set the initial probabilities to the first segment since that is
% uniformly distributed. This is probably wrong...
% initial_probabilities = ones(number_of_matches, number_of_matches) ./ number_of_matches;

% First set of transitions are distances from zero, which favours matches
% that are closer to the start of the target rhythm.
initial_probabilities = segment_transition_probs(:,:,1);

[match_path, loglikelihood] = Fviterbi(initial_probabilities, sorted_location_probs, segment_transition_probs(:,:,2:end));

linear_index = (0 : number_of_segments - 1) * number_of_matches + match_path;

fprintf('suggested segment sequence %s\n', sprintf('%d ', sorted_locations(linear_index)));

end
