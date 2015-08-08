% -*- Octave -*-
function [ precision, recall, f_score ] = evaluate_beat_file(beatMarkerFilePath, annotationFilePath, evaluation)
%evaluate_beat_file Compares the beat times of a computed marker file against an annotation.
% Retrieve the times of nominated beats from an annotation file & return
% the match scores falling within a precision window specified in seconds.
%
% $Id: evaluate_beat_file.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

[annotationTimes, annotationMarkers] = annotated_beats(annotationFilePath);

[beatMarkerTimes, beatMarkers] = read_quaero_beat_markers(beatMarkerFilePath);

if (evaluation.isAnnotationFiltered()) 
    % Annotation filtering specifies the way in which (using a function) the markers are used to
    % identify times to be matched. This means there are two transformations applied, to the
    % annotations and to the beat markers.
    [filteredAnnotationTimes, filteredMarkerTimes] = evaluation.annotationFilter(annotationTimes, annotationMarkers, beatMarkerTimes, beatMarkers);
    fprintf('Num of annotations %d, marker times %d\n', ...
        length(annotationTimes), length(beatMarkerTimes));
    fprintf('Num of filtered annotations %d, filtered marker times %d\n', ...
        length(filteredAnnotationTimes), length(filteredMarkerTimes));
else
    filteredAnnotationTimes = annotationTimes;
    filteredMarkerTimes = beatMarkerTimes;
end

if (evaluation.isRelativePrecision())
    absolutePrecisionWindow = precision_window_of_times(filteredAnnotationTimes, evaluation.precisionWindow);
else
    absolutePrecisionWindow = evaluation.precisionWindow;
end

%fprintf('absolutePrecisionWindow size %s, filteredAnnotationSize %s filteredMakerTimes %s\n', ...
%    sprintf('%d ', size(absolutePrecisionWindow)), sprintf('%d ', size(filteredAnnotationTimes)), sprintf('%d ', size(filteredMarkerTimes)));
%plot_min_distance(sprintf('%s %s', pathname_name(annotationFilePath) annotation_filter)
%		       beatMarkerTimes, filtered_annotationTimes, precision_window)
               
% Filter out times before and after the annotation time extrema.
timeLimitedMarkers = prune_outliers(filteredMarkerTimes, ...
						   filteredAnnotationTimes(1) - absolutePrecisionWindow(end), ...
						   filteredAnnotationTimes(end) + absolutePrecisionWindow(1));
                       
[ precision, recall, f_score ] = evaluate_beat_times(timeLimitedMarkers, filteredAnnotationTimes, absolutePrecisionWindow);

end

