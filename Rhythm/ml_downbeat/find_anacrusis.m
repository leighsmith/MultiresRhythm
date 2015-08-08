function [ anacrusis, minimum_distance ] = find_anacrusis( annotated_beat_filepath, computed_beat_filepath )
%find_anacrusis Returns the anacrusis (in beats) of the computed beats, according to the annotated downbeats.
% $Id$
%

annotated_downbeats = downbeat_times(annotated_beat_filepath);
first_two_downbeats = annotated_downbeats(1:2);
computed_beat_times = read_beats(tilde_expand(computed_beat_filepath));
[minimum_distance, index] = min(min(abs(vector_distance(first_two_downbeats, computed_beat_times))));

% Hardwired to 4/4
beats_per_measure = 4;
anacrusis = mod((index - 1), beats_per_measure);

end

