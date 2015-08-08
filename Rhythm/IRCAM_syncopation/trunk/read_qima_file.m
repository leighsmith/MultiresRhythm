function [ beat_times, beat_markers ] = read_qima_file( filepath )
% Read the given XML file to return an array of beat markers
% $Id$

marker_document = xmlread(tilde_expand(filepath));
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
        % beat_index = str2num(beat_type.getAttribute('beat').toCharArray());
        % beat_time = str2double(marker_node.getAttribute('time').toCharArray());
        % Octave is more forgiving in conversion between Java and Octave strings.
        % Perhaps use char() for matlab and Octave compatibility?
        beat_index = str2num(beat_type.getAttribute('beat'));
        beat_time = str2double(marker_node.getAttribute('time'));
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


