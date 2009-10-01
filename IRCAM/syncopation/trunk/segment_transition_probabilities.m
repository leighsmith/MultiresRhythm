function [ segment_transition_probs ] = segment_transition_probabilities ( segments, segment_locations )
%segment_transition_probabilities Return the probabilities of transitions
%between segments based on the distance between the query segments.
% $Id$

[number_of_matches, number_of_segments ] = size(segment_locations);
segment_transition_probs = zeros(number_of_matches, number_of_matches, number_of_segments);
next_segment_location = zeros(1, number_of_matches);

% Determine the distances between ends of segments and the start of the matches to the next segments.
segment_lengths = segments(:,2) - segments(:,1); % Use the length of the query segment in case the segments are not contiguous.
% Create the transitions with the first transition matrix from 0 to the
% first locations. The second transition matrix from the distance between the first locations + segment length to
% the second locations, etc.
for segment_index = 1 : number_of_segments
    segment_distances = vector_distance(next_segment_location, segment_locations(:,segment_index));
    % Convert from distances to probabilities (summing to 1 across each row) by inverting
    % the probability so that small distances become higher probabilities.
    reverse_segment_distances = repmat(max(segment_distances')' + min(segment_distances')', [1, number_of_matches]) - segment_distances;
    segment_transition_probs(:,:,segment_index) = reverse_segment_distances ./ repmat(sum(reverse_segment_distances')', [1, number_of_matches]);
    % segment_transition_probs(:,:,segment_index) = (1 - segment_distances ./ repmat(sum(segment_distances')', [1, number_of_matches])) ./ (number_of_matches - 1);
    if segment_index == 3
        next_segment_location
        segment_locations(:,segment_index)
        segment_distances
        plot(reverse_segment_trans(2,:));
        figure()
        plot(segment_transition_probs(2,:,segment_index));
        figure();
    end
    next_segment_location = segment_locations(:,segment_index) + segment_lengths(segment_index);
end

segment_lengths'

end

