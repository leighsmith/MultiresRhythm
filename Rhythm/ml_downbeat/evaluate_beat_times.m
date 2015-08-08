% -*- Octave *-*
function [ precision, recall, f_score ] = evaluate_beat_times (computedBeatTimes, annotationTimes, precisionWindow)
%evaluate_beat_times Return the precision, recall and f_scores given the times of computed and annotated beats.
%% precisionWindow can be scalar or a vector of annotationTimes length.
%% Recall = number correct / number annotated => determines deletions
%% Precision = number correct / number computed => determines additions
%% F_score = Single combined value of precision and recall.
%
% $Id: evaluate_beat_times.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation
% purposes.

if (isscalar(precisionWindow))
    precisionWindows = precisionWindow;
else
    precisionWindows = repmat(precisionWindow', length(computedBeatTimes), 1);
end

% Calculate those vector distances within the precision window.
within_precision = vector_distance(computedBeatTimes, annotationTimes) < precisionWindows;
matches_per_annotation = sum(within_precision);
matching_annotations_per_marker = sum(within_precision');
duplicated_matches = sum(matching_annotations_per_marker > 1);

%% remove any annotations that match a marker more than once
number_correct = sum(matches_per_annotation > 0) - duplicated_matches;

recall = number_correct / length(annotationTimes);
precision = number_correct / length(computedBeatTimes);
f_score = (2 * precision * recall) / (precision + recall + eps);

fprintf('number of annotations %d, number of markers within annotation range %d, number correct %d\n', ...
	    length(matches_per_annotation), length(computedBeatTimes), floor(number_correct));
fprintf('number of annotations matching more than one marker %d\n', floor(duplicated_matches));

%% fprintf('first 10 annotations ~a\n' (.subseq annotationTimes 0 9))
%% fprintf('last 10 annotations ~a\n' (.subseq annotationTimes (- (.length annotationTimes) 10)))
%imagesc(within_precision)

end

