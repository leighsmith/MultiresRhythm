function [ output_args ] = bad_beat_tracking( anacrusis, annotated_downbeat_files )
%bad_beat_tracking Summary of this function goes here
%   Detailed explanation goes here

beat_track_rejects = annotated_downbeat_files(find(anacrusis(: , 2) < 0.5));
fprintf('Low F-score, suggest rejecting:\n');
for i = 1 : length(beat_track_rejects)
    fprintf('%s\n', beat_track_rejects{i})
end


end

