function [ original_song_scores, reestimated_song_scores ] = evaluate_corpus_reestimation_tempo( analysis_dir, reestimation_dir, annotated_tempo_file )
%evaluate_corpus_reestimation_tempo Summary of this function goes here
%   Detailed explanation goes here

original_song_scores = evaluate_corpus_tempo(analysis_dir, annotated_tempo_file);
reestimated_song_scores = evaluate_corpus_tempo(reestimation_dir, annotated_tempo_file);

fprintf('Accuracy measure for %s tempo errors, correct: %d/%d\n', ...
        analysis_dir, sum(original_song_scores), length(original_song_scores));

fprintf('Accuracy measure for %s tempo errors, correct: %d/%d\n', ...
        reestimation_dir, sum(reestimated_song_scores), length(reestimated_song_scores));

end

