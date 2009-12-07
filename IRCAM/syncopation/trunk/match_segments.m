function [ sorted_locations, sorted_location_probs ] = match_segments ( query_odf, target_odf, segments, matching_tolerance )
%match_segments Match segments of the query ODF against the target ODF
% $Id$

use_correlation_matches = true;

number_of_segments = size(segments, 1);
number_of_matches = 20;
segment_location_probs = zeros(number_of_matches, number_of_segments);
segment_locations = zeros(number_of_matches, number_of_segments);
target_odf_length = length(target_odf);

if (matching_tolerance > 0)
    widened_target_odf = gaussianRhythm(target_odf, matching_tolerance);
else
    widened_target_odf = target_odf;
end

% Match each query segment against the target, accumulate the locations and probabilities.
for segment_index = 1 : number_of_segments
    segment_range = segments(segment_index, 1) : segments(segment_index, 2);
    query_segment = query_odf(segment_range);
    if (matching_tolerance > 0)
        % Convolve against a narrow Gaussian to broaden the matching
        widened_query_segment = gaussianRhythm(query_segment, matching_tolerance);
    else
        widened_query_segment = query_segment;
    end
    % plot(query_segment);
    % figure();
    [peak_match_indices, peak_match_values] = cross_correlation_match(widened_query_segment ./ max(widened_query_segment),...
        widened_target_odf ./ max(widened_target_odf), number_of_matches);
    segment_locations(1:length(peak_match_values), segment_index) = peak_match_indices;
    segment_location_scores = zeros(number_of_matches, 1);
    if(use_correlation_matches)
        segment_location_probs(1:length(peak_match_values), segment_index) = peak_match_values ./ sum(peak_match_values);
    else
        for match_index = 1 : length(peak_match_values)
            if(segment_range(end) + peak_match_indices(match_index) < target_odf_length)
                target_segment = target_odf(segment_range + peak_match_indices(match_index));
                segment_location_scores(match_index) = match_score(query_segment, target_segment, matching_tolerance);
            end
        end
        segment_location_probs(:, segment_index) = segment_location_scores ./ sum(segment_location_scores);
    end
    fprintf('segment %d:%d matches (%s)\n', segments(segment_index,1), segments(segment_index,2), sprintf('%d ', peak_match_indices));
    fprintf('peak correlation match values: %s\n', sprintf('%.3f ', peak_match_values));
    fprintf('match scores: %s\n', sprintf('%.3f ', segment_location_scores));
    fprintf('segment location probabilities: %s\n\n', sprintf('%.4f ', segment_location_probs(:, segment_index)));

    % plot_correlation_match(query_segment, target_odf, peak_match_indices(1));
    % pause();
end

% Order by the locations of the query segments in the target.
[sorted_locations, sorted_indices] = sort(segment_locations);

% Need to convert the column indices into a linear index.
sorted_linear_indices = sorted_indices + repmat((0:number_of_segments-1) * number_of_matches, number_of_matches, 1);

% Now we can retrieve the segment location probabilities as they were
% sorted into ascending location order.
sorted_location_probs = segment_location_probs(sorted_linear_indices);


end

