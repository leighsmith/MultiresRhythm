function plot_correlation_match( query_rhythm, target_rhythm, shift_by )
%plot_correlation_match Plot the query and target rhythms aligned by 
%shifting the query forward by shift_by samples.

normalised_target = target_rhythm ./ max(target_rhythm) * 2;
normalised_query = query_rhythm ./ max(query_rhythm);
if(length(normalised_target) > length(normalised_query) + shift_by)
    plot(1 : length(normalised_target), normalised_target, ...
         1 : length(normalised_target), ...
        [zeros(1, shift_by) normalised_query zeros(1, length(normalised_target) - length(normalised_query) - shift_by)]);
else
    plot(1 : length(normalised_target), normalised_target, ...
         1 : length(normalised_target), ...
        [zeros(1, shift_by) normalised_query(1 : length(normalised_target) - shift_by)]);
end

end

