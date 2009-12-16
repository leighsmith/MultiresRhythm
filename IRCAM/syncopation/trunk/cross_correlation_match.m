% -*- Octave -*-
function [ selected_peak_indices, selected_peak_values ] = cross_correlation_match(query_vector, target_vector, highest_correlations)
%cross_correlation_match Return the samples that are the highest aligning matches between the two vectors.
% highest_correlations indicates how many matches to return, 
% $Id$

if (nargin < 3)
    highest_correlations = 5;
end

[ shifts, sorted_peak_lag_values ] = correlation_alignments(query_vector, target_vector);

% Make all -ve shifts index from the start of the target_vector, leave the +ve shifts untouched.
folded_shifts = mod(length(target_vector) + shifts, length(target_vector));
% TODO Remove the non-unique folded peak indices.
highest_unique_indices = 1 : min(length(folded_shifts), highest_correlations);
selected_peak_indices = folded_shifts(highest_unique_indices);
selected_peak_values = sorted_peak_lag_values(highest_unique_indices);

end

