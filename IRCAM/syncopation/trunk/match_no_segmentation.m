function [ match_locations, segments, single_match_measure ] = match_no_segmentation( query_odf, target_odf, sample_rate )
%match_no_segmentation match the entire query using cross correlation
%   This aims to test how good the cross correlation matching is, without segmentation.

% matching_tolerance = 0;
matching_tolerance = 0.1451 * sample_rate; % 145 milliseconds, computed to samples.

segments = [1, length(query_odf)];

[sorted_locations, sorted_location_probs] = match_segments(query_odf, target_odf, segments, matching_tolerance);

beyond_odf_artifact = sorted_locations > 100;
filtered_locations = sorted_locations(beyond_odf_artifact);

[single_match_measure, argmax] = max(sorted_location_probs(beyond_odf_artifact));

match_locations = filtered_locations(argmax);

% An average of all segment matches. TODO This ignores the distances the selected segments are apart.
fprintf('locations of matched path %s\n', sprintf('%d ', match_locations));
fprintf('location probabilities of most likely segment sequence %s\n', sprintf('%.4f ', single_match_measure));

end

