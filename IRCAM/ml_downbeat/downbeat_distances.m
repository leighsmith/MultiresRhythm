function [ distance ] = downbeat_distances( annotated_beat_filepath, computed_beat_filepath, beat_phase )
%downbeat_distances Compute the distance between nearest downbeats of the
%two files.
% $Id$
%

annotated_downbeats = downbeat_times(annotated_beat_filepath);
computed_downbeats = downbeat_times(computed_beat_filepath, beat_phase);
%annotated_downbeats(1 : 5)
%computed_downbeats(1 : 5)
distance = min(min(abs(vector_distance(annotated_downbeats, computed_downbeats))));

end

