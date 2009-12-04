function [ match_locations, segments, single_match_measure ] = match_rhythm_odf( query_odf, target_odf, sample_rate )
%match_rhythm_odf Returns the possible locations and probabilities of
% matching segments of the query against the target.
% $Id$

segments = segment_odf(query_odf, sample_rate);

number_of_segments = size(segments, 1);

[sorted_locations, sorted_location_probs] = match_segments(query_odf, target_odf, segments);

%figure();
%imagesc(sorted_location_probs);
%sorted_locations

number_of_matches = size(sorted_locations, 1);

segment_transition_probs = segment_transition_probabilities(segments, sorted_locations);

% stay_on_path = repmat(eye(number_of_matches), [1, 1, number_of_segments-1]);
% go_up = repmat(circshift(eye(number_of_matches), [0, 1]), [1, 1, number_of_segments-1]);
% go_down = repmat(circshift(eye(number_of_matches), [0, -1]), [1, 1, number_of_segments-1]);

%figure();
%imagesc(segment_transition_probs(:,:,2));
%segment_transition_probs(:,:,2)

% Weight the initial probabilities toward earlier rhythmic structures.
% First set of transitions are distances from zero, which favours matches
% that are closer to the start of the target rhythm.
% initial_probabilities = segment_transition_probs(:,:,1);
% Alternative initial transitions which gives equal weight to the match
% anywhere across the target.
% Set the initial probabilities to the first segment since that is
% uniformly distributed. This is probably wrong...
initial_probabilities = ones(number_of_matches, number_of_matches) ./ number_of_matches;

% figure();
% imagesc(initial_probabilities);

[match_path, loglikelihood] = Fviterbi(initial_probabilities, sorted_location_probs, segment_transition_probs(:,:,2:end));
% [match_path, loglikelihood] = Fviterbi(initial_probabilities, sorted_location_probs, go_up);

exp(loglikelihood)

linear_index = (0 : number_of_segments - 1) * number_of_matches + match_path;
match_locations = sorted_locations(linear_index);

% An average of all segment matches. This ignores the distances the
% segments are apart.
fprintf('location probabilities of most likely segment sequence %s\n', sprintf('%.3f ', sorted_location_probs(linear_index)));
single_match_measure = sum(sorted_location_probs(linear_index)) / number_of_segments;

end
