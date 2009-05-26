function [ beat_times, beat_indices ] = annotated_beats( ircam_annotation_path )
% Retrieve the times of beats and downbeats, skipping beat = 0, which are
% tatums, in the IRCAM annotation convention.

beats_and_times = read_ircam_annotation(ircam_annotation_path);
beat_indices = find(beats_and_times(:, 1) > 0);
beat_times = beats_and_times(beat_indices, 2);

end

