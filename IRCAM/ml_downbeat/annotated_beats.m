function [ beat_times, beat_markers ] = annotated_beats( ircam_annotation_path )
% Retrieve the times of beats and downbeats, skipping beat = 0, which are
% tatums, in the IRCAM annotation convention.
%
% $Id: annotated_beats.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

[beat_times, beat_markers] = read_quaero_beat_markers(ircam_annotation_path);
beat_indices = find(beat_markers(:) > 0);
beat_times = beat_times(beat_indices);
beat_markers = beat_markers(beat_indices);

end

