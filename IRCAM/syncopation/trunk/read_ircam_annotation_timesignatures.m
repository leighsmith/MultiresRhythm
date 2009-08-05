function [ segment_timesignatures, hierarchy ] = read_ircam_annotation_timesignatures( filepath, segment_name )
%read_ircam_annotation_timesignatures Read the time signatures in the annotation file and return an
%array of the beats per measure and the hierarchy of subdivisions of the measure.
% $Id$

if nargin < 2
    segment_name = 'marker';
end

segment_document = xmlread(filepath);
segment_root = segment_document.getDocumentElement();
segments = segment_root.getElementsByTagName(segment_name);

timesignatures_and_times = zeros(segments.getLength(), 2);
write_index = 1;

for segment_index = 0 : segments.getLength() - 1
    segment_node = segments.item(segment_index);
    comment = char(segment_node.getAttribute('comment'));
    time = str2double(char(segment_node.getAttribute('time')));

    beats_per_measure = 0;
    switch comment
    case 'timesignature-4/4'
        beats_per_measure = 4;
        hierarchy = [2 2 2 2];
    case 'timesignature-6/8'
        beats_per_measure = 6;
        hierarchy = [2 3 2];
    case 'timesignature-12/8'
        beats_per_measure = 12;
        hierarchy = [2 2 3 2];
    case 'timesignature-3/4'
        beats_per_measure = 3;
        hierarchy = [3 2 2];
    end
    if beats_per_measure ~= 0
       % fprintf('beat %d time %d\n', beats_per_measure, time)
       % collect beats numbers and times in seconds into the dispatch matrix.
       timesignatures_and_times(write_index, :) = [beats_per_measure, time];
       write_index = write_index + 1;
    end
end
   
segment_timesignatures = timesignatures_and_times(1 : write_index - 1, :);

end

