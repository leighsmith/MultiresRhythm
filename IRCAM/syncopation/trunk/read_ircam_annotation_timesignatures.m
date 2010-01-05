function [ segment_timesignatures, hierarchy ] = read_ircam_annotation_timesignatures( filepath)
%read_ircam_annotation_timesignatures Read the time signatures in the annotation file and return an
%array of the beats per measure and the hierarchy of subdivisions of the measure.
% $Id$

segment_document = xmlread(filepath);
segment_root = segment_document.getDocumentElement();
timesignatures = segment_root.getElementsByTagName('timesignature');

timesignatures_and_times = zeros(timesignatures.getLength(), 2);
write_index = 1;

hierarchy = [];
        
for timesignature_index = 0 : timesignatures.getLength() - 1
    timesignature_node = timesignatures.item(timesignature_index);
    segment_node = timesignature_node.getParentNode();
    comment = char(segment_node.getAttribute('comment'));
    time = str2double(char(segment_node.getAttribute('time')));

    beats_per_measure = str2double(char(timesignature_node.getAttribute('beatspermeasure')));
    beatduration = str2double(char(timesignature_node.getAttribute('beatduration')));
    comment = char(timesignature_node.getAttribute('comment'));
    
    % Kludge to get around ircambeat
    if(strcmp(comment, 'with triplets') && beats_per_measure == 4 && beatduration == 4)
        beats_per_measure = 6;
        beatduration = 8;
    end
    
    switch beats_per_measure
    case 4
        hierarchy = [2 2 2 2];
    case 6
        hierarchy = [2 3 2];
    case 12
        hierarchy = [2 2 3 2];
    case 3
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

