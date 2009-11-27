function [ match_locations, segments, single_match_measure ] = match_no_segmentation( query_odf, target_odf, sample_rate  )
%match_no_segmentation match the entire query using cross correlation
%   Detailed explanation goes here

segments = [1, length(query_odf)];

[sorted_locations, sorted_location_probs] = match_segments(query_odf, target_odf, segments)

beyond_odf_artifact = sorted_locations > 100;
filtered_locations = sorted_locations(beyond_odf_artifact);

[single_match_measure, argmax] = max(sorted_location_probs(beyond_odf_artifact))

match_locations = filtered_locations(argmax);

end

