function [ original_song_scores, reestimated_song_scores ] = evaluate_corpus_reestimation_tempo( analysis_dir, reestimation_dir, annotated_tempo_file )
%evaluate_corpus_reestimation_tempo Compares the files in the analysis and reestimated
%analysis directories for their match to the annotated tempi.

[original_song_scores, original_octave_errors] = evaluate_corpus_tempo(analysis_dir, annotated_tempo_file);
cellfun(@disp, original_octave_errors, 'UniformOutput', false);
[reestimated_song_scores, reestimated_octave_errors] = evaluate_corpus_tempo(reestimation_dir, annotated_tempo_file);
cellfun(@disp, reestimated_octave_errors, 'UniformOutput', false);

fprintf('Accuracy measure for %s tempo errors, correct: %d/%d, octave_errors %d\n', ...
        analysis_dir, sum(original_song_scores), length(original_song_scores), length(original_octave_errors));

fprintf('Accuracy measure for %s tempo errors, correct: %d/%d, octave_errors %d\n', ...
        reestimation_dir, sum(reestimated_song_scores), length(reestimated_song_scores), length(reestimated_octave_errors));

end

