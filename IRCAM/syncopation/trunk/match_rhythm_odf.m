function [ sorted_locations, sorted_location_probs, segment_transition_probs ] = match_rhythm_odf( query_odf, target_odf, sample_rate )
%match_rhythm_odf Returns the possible locations and probabilities of
% matching segments of the query against the target.
% $Id$

segments = segment_odf(query_odf, sample_rate);

number_of_segments = size(segments,1);
number_of_matches = 20;
segment_location_probs = zeros(number_of_matches, number_of_segments);
segment_locations = zeros(number_of_matches, number_of_segments);
match_from = 0; % TODO should be 1?

% Match each query segment against the target, accumulate the locations and probabilities.
for segment_index = 1 : number_of_segments
    query_segment = query_odf(segments(segment_index,1) : segments(segment_index,2));
    % Convolve against a narrow Gaussian to broaden the matching
    query_segment = gaussianRhythm(query_segment, 100);
    % plot(query_segment);
    % figure();
    [peak_match_indices, peak_match_values] = cross_correlation_match(query_segment ./ max(query_segment), target_odf ./ max(target_odf), number_of_matches, match_from);
    fprintf('segment %d:%d matches (%s)\n', segments(segment_index,1), segments(segment_index,2), sprintf('%d ', peak_match_indices));
    segment_location_probs(1:length(peak_match_values), segment_index) = peak_match_values ./ sum(peak_match_values);
    segment_locations(1:length(peak_match_values), segment_index) = peak_match_indices;
    % match_from = min(peak_match_indices) % No point looking before the earliest previous segment match.
    % plot_correlation_match(query_segment, target_odf, peak_match_indices(1));
    % pause();
end

% Order by the locations of the query segments in the target.
[sorted_locations, sorted_indices] = sort(segment_locations);
sorted_location_probs = segment_location_probs(sorted_indices);

segment_transition_probs = segment_transition_probabilities(segments, sorted_locations);

imagesc(segment_transition_probs(:,:,2));
% imagesc(segment_location_probs(sorted_indices));

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
