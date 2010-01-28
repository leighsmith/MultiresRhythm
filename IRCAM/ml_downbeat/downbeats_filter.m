function [ filtered_annotation_times, filtered_marker_times ] = downbeats_filter( annotation_times, annotation_markers, marker_times, beat_markers )
%downbeats_filter Returns the annotation_times and marker_times filtered of
%all but the downbeats.
%
% $Id: downbeats_filter.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

annotation_downbeat_indices = find(annotation_markers == 1);
filtered_annotation_times = annotation_times(annotation_downbeat_indices);
marker_downbeat_indices = find(beat_markers == 1);
filtered_marker_times = marker_times(marker_downbeat_indices);

end

