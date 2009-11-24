function [ segment_transition_probs ] = segment_transition_probabilities ( segments, segment_locations )
%segment_transition_probabilities Return the probabilities of transitions
%between segments based on the distance between the query segments.
% $Id$

[number_of_matches, number_of_segments] = size(segment_locations);
segment_transition_probs = zeros(number_of_matches, number_of_matches, number_of_segments);
prev_segment_end = zeros(1, number_of_matches);
epsilon = 0.0001;

% Determine the distances between ends of segments and the start of the matches to the next segments.
segment_lengths = segments(:,2) - segments(:,1); % Use the length of the query segment in case the segments are not contiguous.
% Create the transitions with the first transition matrix from 0 to the first locations. 
% The subsequent transition matrices are from the distance between the first locations + segment length to
% the second locations, etc.
for segment_index = 1 : number_of_segments
    segment_distances = vector_distance(segment_locations(:,segment_index), prev_segment_end);
    abs_segment_distances = abs(segment_distances);
    % Convert from distances to probabilities (summing to 1 across each row) by inverting
    % the probability so that small distances become higher probabilities.
    % We add the minimum value to distinguish 0 distance from the least
    % value.
    max_distance = repmat(max(abs_segment_distances')' + min(abs_segment_distances')', [1, number_of_matches]);
    inverted_segment_distances = max_distance - abs_segment_distances;
    % Any negative distances (i.e the segments are reversed in time) should become (near to) zero probabilities.
    % inverted_segment_distances(segment_distances < 0) = epsilon;
    segment_pair_probs = inverted_segment_distances ./ repmat(sum(inverted_segment_distances')', [1, number_of_matches]);
    % segment_transition_probs(:,:,segment_index) = (1 - segment_distances ./ repmat(sum(segment_distances')', [1, number_of_matches])) ./ (number_of_matches - 1);

    % transpose since Fviterbi retrieves the state transition matrix using the state indexed by columns.
    segment_transition_probs(:,:,segment_index) = segment_pair_probs';
    if segment_index == 0
        prev_segment_end
        segment_locations(:,segment_index)
        segment_distances
        figure();
        plot(segment_transition_probs(2,:,segment_index));
    end
    prev_segment_end = segment_locations(:,segment_index) + segment_lengths(segment_index);
end

segment_lengths'

end

