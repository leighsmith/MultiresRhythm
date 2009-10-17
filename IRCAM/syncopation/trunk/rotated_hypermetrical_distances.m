function [ euclidean_distances ] = rotated_hypermetrical_distances(hypermeters)
%rotated_hypermeterical_distances Return Euclidean distances from best
%rotations of the hypermetrical phrases.
% $Id:$

num_tracks = size(hypermeters, 1);
euclidean_distances = zeros(num_tracks);

for queryTrackIndex = 1 : num_tracks
    query_hypermeter = hypermeters(queryTrackIndex,:);
    for trackIndex = 1 : num_tracks
        best_alignment = hypermeter_align(query_hypermeter, hypermeters(trackIndex, :));
        euclidean_distances(queryTrackIndex, trackIndex) = sum((hypermeters(trackIndex,:) - best_alignment) .^ 2);
    end
end

end

