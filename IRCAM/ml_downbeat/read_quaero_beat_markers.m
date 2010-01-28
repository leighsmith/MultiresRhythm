% -*- Octave *-*
function [ beat_times, beat_markers ] = read_quaero_beat_markers( filepath )
%read_quaero_beat_markers Read a Quaero music description XML file to return an array of beat marker times.
% The second element returned is the location within measure for each
% beat.
%
% $Id: read_quaero_beat_markers.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

marker_document = xmlread(filepath);
marker_root = marker_document.getDocumentElement();
markers = marker_root.getElementsByTagName('segment');

% These are overestimates, as not all elements will be true markers
beat_markers = zeros(markers.getLength(), 1);
beat_times = zeros(markers.getLength(), 1);
write_index = 1;
for marker_index = 0 : markers.getLength() - 1
    marker_node = markers.item(marker_index);
    beattypes = marker_node.getElementsByTagName('beattype');
    if beattypes.getLength()
        beat_type = beattypes.item(0); % perhaps getFirstChild()
        % getAttribute returns Java Strings, convert them to standard
        % Matlab integers and floats.
        beat_index = str2num(beat_type.getAttribute('beat').toCharArray());
        beat_time = str2double(marker_node.getAttribute('time').toCharArray());
        if(beat_index ~= 0)
            % fprintf('beat %d time %d\n', beat_index, beat_time)
            % collect beats numbers and times in seconds.
            beat_times(write_index, :) = beat_time;
            beat_markers(write_index, :) = beat_index;
            write_index = write_index + 1;
        end
    end
end
% Trim the returned arrays to the number of markers actually written.
beat_markers = beat_markers(1 : write_index - 1);
beat_times = beat_times(1 : write_index - 1);


