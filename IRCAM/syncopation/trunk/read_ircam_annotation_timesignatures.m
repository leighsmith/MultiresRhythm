function [ marker_timesignatures ] = read_ircam_annotation_timesignatures( filepath )
%read_ircam_annotation_timesignatures Read the time signatures in the annotation file and return an
%array of the beats per measure.

marker_name = 'marker';
marker_document = xmlread(filepath);
marker_root = marker_document.getDocumentElement();
markers = marker_root.getElementsByTagName(marker_name);

timesignatures_and_times = zeros(markers.getLength(), 2);
write_index = 1;

for marker_index = 0 : markers.getLength() - 1
    marker_node = markers.item(marker_index);
    % Java toCharArray method spits out a column array, so we (sigh) have to transpose it.
    comment = marker_node.getAttribute('comment').toCharArray().';
    time = str2double(marker_node.getAttribute('time').toCharArray());

    beats_per_measure = 0;
    switch comment
    case 'timesignature-4/4'
        beats_per_measure = 4;
    case 'timesignature-6/8'
        beats_per_measure = 6;
    case 'timesignature-12/8'
        beats_per_measure = 12;
    case 'timesignature-3/4'
        beats_per_measure = 3;
    end
    if beats_per_measure
       % fprintf('beat %d time %d\n', beats_per_measure, time)
       % collect beats numbers and times in seconds into the dispatch matrix.
       timesignatures_and_times(write_index, :) = [beats_per_measure, time];
       write_index = write_index + 1;
    end
end
   
marker_timesignatures = timesignatures_and_times(1 : write_index - 1, :);

end

