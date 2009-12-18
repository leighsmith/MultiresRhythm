function [ beat_times, beat_markers ] = read_beats( beat_marker_filepath )
%read_beats Retrieve the times of beats and downbeats, skipping beat = 0, which are
% tatums, in the IRCAM annotation convention.
% $Id$

[beat_times, beat_markers] = read_ircam_marker_times(beat_marker_filepath);
beat_indices = find(beat_markers(:) > 0);
beat_times = beat_times(beat_indices);
beat_markers = beat_markers(beat_indices);

end

