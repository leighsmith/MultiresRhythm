function [ reestimated_song_scores, original_song_scores ] = evaluate_corpus_reestimation( analysis_dir, reestimation_dir, annotation_dir )
%beat_tracking_improvement Produces comparison of performance before and
%after reestimating the tempo for the given patterns identified as octave errors.

analysed_files = make_dataset(reestimation_dir, '.wav.markers.xml', 0);

% Evaluate how well the beats match the annotations.
evaluation = Evaluation();
% evaluation.annotationFilter = @downbeats_filter;
evaluation.annotationFilter = ''; % Check accuracy of all beats.
% evaluation.annotationFilter = @halfbeats_filter; % Check accuracy of all beats.
evaluation.relativePrecision = true;
evaluation.precisionWindow = 0.15; % Within 15% of the annotated beat position.

% analysis_dir for all good beat tracked analyses.
original_song_scores = evaluate_beat_corpus(analysed_files, evaluation, analysis_dir, annotation_dir);
reestimated_song_scores = evaluate_beat_corpus(analysed_files, evaluation, reestimation_dir, annotation_dir);

% Generate auditions
% audition_rwc_corpus(reestimation_dir, false)

display_scores(analysis_dir, analysed_files, original_song_scores, evaluation.precisionWindow);
display_scores(reestimation_dir, analysed_files, reestimated_song_scores, evaluation.precisionWindow);

end

function display_scores(corpus_name, filenames, scoresPerTrack, precisionWindow)
    [number_of_octave_errors, octave_error_types] = octave_errors(scoresPerTrack);
    
    meanScores = mean(scoresPerTrack);
    fprintf('Accuracy measure for %s precision window %.3f octave errors %d/%d Mean Precision %.3f Recall %.3f F-score %.3f\n', ...
        corpus_name, precisionWindow, number_of_octave_errors, size(scoresPerTrack, 1), meanScores(1), meanScores(2), meanScores(3));

    octave_error_files = filenames(find(octave_error_types ~= 0));
    fprintf('Files with octave errors:\n');
    cellfun(@disp, octave_error_files, 'UniformOutput', false);
end
