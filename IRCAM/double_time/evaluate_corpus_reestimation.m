function [ reestimated_song_scores, original_song_scores ] = evaluate_corpus_reestimation( analysis_dir )
%beat_tracking_improvement Produces comparison of performance before and
%after reestimating the tempo for the given patterns identified as octave errors.

root_dir = '~/Research/Data/IRCAM-Beat/RWC/';

analysed_files = make_dataset([root_dir 'ReestimatedTempo'], '.wav.markers.xml', 0);

% Evaluate how well the beats match the annotations.
evaluation = Evaluation();
% evaluation.annotationFilter = @downbeats_filter;
evaluation.annotationFilter = ''; % Check accuracy of all beats.
% evaluation.annotationFilter = @halfbeats_filter; % Check accuracy of all beats.
evaluation.relativePrecision = true;
evaluation.precisionWindow = 0.15; % Within 15% of the annotated beat position.

% analysis_dir for all good beat tracked analyses.
original_song_scores = evaluate_beat_corpus(analysed_files, evaluation, analysis_dir);
reestimated_song_scores = evaluate_beat_corpus(analysed_files, evaluation, 'ReestimatedTempo');

% Generate auditions
% audition_rwc_corpus('ReestimatedTempo', false)

display_scores(analysis_dir, original_song_scores, evaluation.precisionWindow);
display_scores('Reestimated Tempo', reestimated_song_scores, evaluation.precisionWindow);

end

function display_scores(corpus_name, scoresPerTrack, precisionWindow)
    meanScores = mean(scoresPerTrack);
    [number_of_octave_errors, double_time_tracks] = number_of_double_time(scoresPerTrack);
    
    fprintf('Accuracy measure for %s precision window %.3f No of octave errors %d Mean Precision %.3f Recall %.3f F-score %.3f\n', ...
        corpus_name, precisionWindow, number_of_octave_errors, meanScores(1), meanScores(2), meanScores(3));
end

function [number, double_time_tracks] = number_of_double_time(scoresPerTrack)
    % divide recall by precision.
    double_time_tracks = round(scoresPerTrack(:,2) ./ scoresPerTrack(:,1)) == 2;
    % round((reestimated_song_scores(:,2) ./ reestimated_song_scores(:,1)) * 2) / 2
    number = sum(double_time_tracks);
end
