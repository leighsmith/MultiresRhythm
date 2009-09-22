function [ selected_peak_indices, selected_peak_values ] = cross_correlation_match(query_rhythm_function, target_rhythm_function, highest_correlations, start_from)
%cross_correlation_match Return the samples that are the highest aligning matches between the two vectors
% highest_correlations indicates how many matches to return, start_from
% indicates where to begin searching for matches from.
if (nargin < 3)
    highest_correlations = 5;
end

if (nargin < 4)
    start_from = 0;
end

query_target_correlation = xcorr(query_rhythm_function, target_rhythm_function);

%% Since the crosscorrelation rotates the result forward by the target_rhythm_function
%% length, the zeroth lag is at that element.
zeroth_lag = length(target_rhythm_function);

%% Find the highest_correlations number of the highest correlation measures, returning the locations in a
%% vector, in descending correlation order.
peak_indices = find_local_extrema(query_target_correlation, 'max');
peak_lag_values = query_target_correlation(peak_indices);
[sorted_peak_lag_values, sorted_peak_lag_indices] = sort(peak_lag_values, 'descend');

highest_peak_indices = sorted_peak_lag_indices(1 : min(length(sorted_peak_lag_indices), highest_correlations));
selected_peak_indices = abs(peak_indices(highest_peak_indices) - zeroth_lag) + start_from;
selected_peak_values = peak_lag_values(highest_peak_indices);

end

