function [ segment_locations, segment_location_probs, segment_transition_probs ] = match_rhythm_odf( query_odf, target_odf, sample_rate )
%match_rhythm_odf Returns the possible locations and probabilities of
% matching segments of the query against the target.
% $Id$

segments = segment_odf(query_odf, sample_rate);

number_of_segments = size(segments,1);
number_of_matches = 10;
segment_location_probs = zeros(number_of_matches, number_of_segments);
segment_locations = zeros(number_of_matches, number_of_segments);
segment_transition_probs = zeros(number_of_matches, number_of_matches, number_of_segments);
match_from = 0; % TODO should be 1?
segment_lengths = segments(:,2) - segments(:,1) % Use the length of the query segment in case the segments are not contiguous.

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
[sorted_locations, sorted_indices] = sort(segment_locations)
next_segment_location = zeros(number_of_matches);

% Determine the distances between ends of segments and the matches to the next segments.
for segment_index = 1 : number_of_segments
    segment_transition_probs(:,:,segment_index) = vector_distance(sorted_locations(:,segment_index), next_segment_location);
    next_segment_location = sorted_locations(:,segment_index) + segment_lengths(segment_index);
end

segment_transition_probs

end

