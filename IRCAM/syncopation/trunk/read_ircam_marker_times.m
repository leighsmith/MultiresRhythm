function [ marker_times, marker_indices ] = read_ircam_marker_times( beat_marker_filepath )
%read_ircam_marker_times Reads the marker times from the named XML file.
% $Id$

% New version 20090226A just uses the same beattype markers, as
% annotations, named segments.
[marker_times, marker_indices] = read_qima_file(beat_marker_filepath);

end

