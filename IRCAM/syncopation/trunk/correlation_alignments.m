% -*- Octave -*-
function [ shifts, sorted_peak_lag_values ] = correlation_alignments(query_vector, target_vector)
%correlation_alignments Returns the best bivalent lags to align the query
%vector against the target.
%   0 indicates no shift of the query, +1 shifts the query 1 sample
%   forward, to match the first sample of the query with the second sample of the target,
%   -1 shifts the query back 1 sample  so that the second sample of the query matches
%   the first sample of the target.

query_target_correlation = xcorr(query_vector, target_vector);

%% Since the crosscorrelation rotates the result forward by the target_vector
%% length, the zeroth lag is at that element.
zeroth_lag = length(target_vector);

%% Find the highest_correlations number of the highest correlation measures, returning the locations in a
%% vector, in descending correlation order.
peak_indices = find_local_extrema(query_target_correlation, 'max');
peak_lag_values = query_target_correlation(peak_indices);
[sorted_peak_lag_values, sorted_peak_lag_indices] = sort(peak_lag_values, 'descend');

shifts = peak_indices(sorted_peak_lag_indices) - zeroth_lag;

end

