function [ beat_times, beat_markers ] = annotated_beats( ircam_annotation_path )
% Retrieve the times of beats and downbeats, skipping beat = 0, which are
% tatums, in the IRCAM annotation convention.
% $Id$

[beat_times, beat_markers] = read_ircam_annotation(ircam_annotation_path);
beat_indices = find(beat_markers(:) > 0);
beat_times = beat_times(beat_indices);
beat_markers = beat_markers(beat_indices);

end

