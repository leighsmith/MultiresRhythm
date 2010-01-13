function [ distance ] = downbeat_distances( annotated_beat_filepath, computed_beat_filepath )
%downbeat_distances Compute the distance between nearest downbeats of the
%two files.
% $Id$
%

annotated_downbeats = downbeat_times(annotated_beat_filepath);
computed_downbeats = downbeat_times(computed_beat_filepath);
distance = min(min(abs(vector_distance(annotated_downbeats, computed_downbeats))));

end

