function [ selected_peak_indices, selected_peak_values ] = cross_correlation_match(query_vector, target_vector, highest_correlations)
%cross_correlation_match Return the samples that are the highest aligning matches between the two vectors.
% highest_correlations indicates how many matches to return, 
% $Id$

if (nargin < 3)
    highest_correlations = 5;
end

query_target_correlation = xcorr(query_vector, target_vector);

%% Since the crosscorrelation rotates the result forward by the target_vector
%% length, the zeroth lag is at that element.
zeroth_lag = length(target_vector);

%% Find the highest_correlations number of the highest correlation measures, returning the locations in a
%% vector, in descending correlation order.
peak_indices = find_local_extrema(query_target_correlation, 'max');
peak_lag_values = query_target_correlation(peak_indices);
[sorted_peak_lag_values, sorted_peak_lag_indices] = sort(peak_lag_values, 'descend');

folded_peak_indices = abs(peak_indices(sorted_peak_lag_indices) - zeroth_lag);
% TODO Remove the non-unique folded peak indices.
highest_unique_indices = 1 : min(length(folded_peak_indices), highest_correlations);
selected_peak_indices = folded_peak_indices(highest_unique_indices);
selected_peak_values = sorted_peak_lag_values(highest_unique_indices);

end

