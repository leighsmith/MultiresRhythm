% -*- Octave -*-
function [ songScores ] = evaluate_beat_corpus( corpus_song_names, evaluation, analysis_dir )
%evaluate_beat_with_corpus Evaluate the list of tracks in corpus for the given precision_window
% Evaluate the beats contained in the annotation and analysis files of the
% given corpus.
%
% Inputs:
% corpus is an instance of Corpus class, evaluation is an instance of Evaluation class.
%
% Outputs:
% songScores is a matrix of precision, recall and F-scores for each song.
%
% $Id: evaluate_beat_with_corpus.m 1441 2009-10-15 09:54:54Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

corpus_directory_root = '~/Research/Data/IRCAM-Beat/RWC/';
songScores = zeros(length(corpus_song_names), 3); % precision, recall, f-score.

for corpus_index = 1 : length(corpus_song_names)
    song_name = basename(corpus_song_names{corpus_index});
    
    annotation_filepath = tilde_expand([corpus_directory_root 'Annotation/' song_name '.beat.xml']);
    beat_marker_filepath = tilde_expand([corpus_directory_root analysis_dir '/' song_name '.wav.markers.xml']);
    
    % print the filename before processing
    fprintf('annotation: %s\n', annotation_filepath)
    fprintf('computed: %s\n', beat_marker_filepath)
    
    [precision, recall, f_score] = evaluate_beat_file(beat_marker_filepath, annotation_filepath, evaluation);
    songScores(corpus_index, :) = [precision recall f_score];

    if (evaluation.isRelativePrecision())
        precisionForm = 'Relative';
    else
        precisionForm = 'Absolute';
    end

    fprintf('%s precision window %.3f precision %.3f recall %.3f f-score %.3f\n\n', ...
        precisionForm, evaluation.precisionWindow, precision, recall, f_score);
    
end

end

