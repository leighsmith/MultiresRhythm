function [ beat_markers_and_times ] = read_ircam_annotation( filepath )
% Read the given file using CXML and DOM to return an array of beat markers"
marker_name = 'marker';
marker_document = xmlread(filepath);
marker_root = marker_document.getDocumentElement();
markers = marker_root.getElementsByTagName(marker_name);

beats_and_times = zeros(markers.getLength(), 2);
write_index = 1;
for marker_index = 0 : markers.getLength() - 1
    marker_node = markers.item(marker_index);
    beattypes = marker_node.getElementsByTagName('beattype');
    if beattypes.getLength()
        beat_type = beattypes.item(0); % perhaps getFirstChild()
        % getAttribute returns Java Strings, convert them to standard
        % Matlab integers and floats.
        beat_index = str2num(beat_type.getAttribute('b').toCharArray());
        beat_time = str2double(marker_node.getAttribute('time').toCharArray());
        if(beat_index ~= 0)
            % fprintf('beat %d time %d\n', beat_index, beat_time)
            % collect beats numbers and times in seconds into the dispatch matrix.
            beats_and_times(write_index, :) = [beat_index, beat_time];
            write_index = write_index + 1;
        end
    end
end
beat_markers_and_times = beats_and_times(1 : write_index - 1, :);


