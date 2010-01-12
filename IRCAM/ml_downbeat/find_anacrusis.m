function [ anacrusis ] = find_anacrusis( annotated_beat_filepath, computed_beat_filepath )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

%   Detailed explanation goes here
annotated_downbeats = downbeat_times(annotated_beat_filepath);
first_two_downbeats = annotated_downbeats(1:2);
[computed_beat_times] = read_beats(tilde_expand(computed_beat_filepath));
distance = vector_distance(first_two_downbeats, computed_downbeats))));

[minimum_distance, index] = min(min(abs(vector_distance(first_two_downbeats, computed_beat_times(1:6)))));

anacrusis = index - 1;

end

