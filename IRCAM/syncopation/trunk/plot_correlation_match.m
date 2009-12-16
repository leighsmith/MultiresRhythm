function plot_correlation_match( query_rhythm, target_rhythm, all_shifts, query_segments )
%plot_correlation_match Plot the query and target rhythms aligned by shifting the query forward by shift_by samples.
% $Id$

normalised_target = reshape(target_rhythm, 1, length(target_rhythm)) ./ max(target_rhythm);
normalised_query = reshape(query_rhythm, 1, length(query_rhythm)) ./ max(query_rhythm);

if(isscalar(all_shifts))
    % fprintf('all_shifts is scalar, need to promote to array\n');
    all_shifts = [all_shifts];
end

if(nargin < 4)
    query_segments = repmat([1, length(query_rhythm)], length(all_shifts), 1);
end

segment_shifts = zeros(length(all_shifts), length(normalised_target));
plot_min_bound = max(min(all_shifts), 1);
plot_max_bound = max(all_shifts) + query_segments(end,2) - query_segments(end,1); 

for shift_index = 1 : length(all_shifts)
    shift_by = all_shifts(shift_index);
    segment = normalised_query(query_segments(shift_index, 1) : query_segments(shift_index, 2));
    
    if(length(normalised_target) > length(segment) + shift_by)
        if(shift_by < 0)
            segment_shifts(shift_index, :) = [segment(abs(shift_by) + 1 : end) zeros(1, length(normalised_target) - length(segment) - shift_by)];
        else
            segment_shifts(shift_index, :) = [zeros(1, shift_by) segment zeros(1, length(normalised_target) - length(segment) - shift_by)];
        end
    else
        segment_shifts(shift_index, :) = [zeros(1, shift_by) segment(1 : length(normalised_target) - shift_by)];
    end
end

% Prepend the row for the original rhythm.
full_shifting = [normalised_target; segment_shifts]';

plot(plot_min_bound : plot_max_bound, full_shifting(plot_min_bound : plot_max_bound, :));
% plot(full_shifting);

segment_legends = cell(size(query_segments, 1) + 1, 1);
segment_legends{1} = 'Target';
for segment_index = 1 : size(query_segments, 1)
    segment_legends{segment_index + 1} = sprintf('Query segment (%d, %d)', query_segments(segment_index, 1), query_segments(segment_index, 2));
end

legend(segment_legends);

end

