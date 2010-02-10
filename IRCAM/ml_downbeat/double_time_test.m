function [ precision, recall, f_score ] = double_time_test( annotation_dir, analysis_dir )
%double_time_test Summary of this function goes here
%   Detailed explanation goes here

% rwc_44 = make_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Analysis'), '.wav.markers.xml');

annotated_files = make_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT'), '.beat.xml');
good_beat_tracking = prune_bad_beat_tracking(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Analysis'), annotated_files);
sound_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Audio/Popular_music/WAV');
corpus_patterns = pattern_for_corpus(good_beat_tracking, sound_directory_root);



% These are the double time examples.
% RM-P004
% RM-P009 % Actually not double time, annotated wrongly.
% RM-P014
% RM-P039
% RM-P045
% RM-P056
% RM-P062
% RM-P066
% RM-P069
% RM-P078
% RM-P082 % Starts on correct tempo then doubles halfway through.
% RM-P086
% RM-P100
octave_errors = [4, 9, 14, 39, 45, 56, 62, 66, 69, 78, 82, 86, 100];

% Find and return the octave_error
octave_error_filenames = cell(1, length(octave_errors));
for j = 1 : length(octave_errors)
    octave_error_filenames{j} = [tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Analysis/') sprintf('RM-P%03d.wav.markers.xml', octave_errors(j))];
end

% octave_error_patterns = pattern_for_corpus(octave_error_filenames, tilde_expand(sound_directory_root));

% 
% quaver_alternation_prob = 1 - cellfun(@quaver_alternation, corpus_patterns);
% freq_alternation_prob = 1 - cellfun(@frequency_alternation, corpus_patterns);
% double_time_prob = quaver_alternation_prob;
% % double_time_prob = freq_alternation_prob;
% % double_time_prob = freq_alternation_prob .* quaver_alternation_prob;
% % double_time_prob = freq_alternation_prob + quaver_alternation_prob;
% threshold = mean(double_time_prob) + std(double_time_prob);

double_time_prob = double_time_of_corpus(good_beat_tracking, sound_directory_root);

% double_time_prob = cellfun(@double_time, corpus_patterns);
% threshold = mean(double_time_prob) + std(double_time_prob);
% Since the double_time_prob is a ratio of half time quaver alternation
% against original time quaver alternation, it must be above 1.0 at a
% minimum for accepting as likely to be double time.
threshold = 1.0;

likely_double_time = find(double_time_prob > threshold);
% double_time_patterns = corpus_patterns(likely_double_time);

% cellfun(@name, double_time_patterns, 'UniformOutput', false)
% cellfun(@name, octave_error_patterns, 'UniformOutput', false)

octave_errors_in_corpus = find_patterns(corpus_patterns, octave_errors);

x = zeros(size(corpus_patterns));
x(octave_errors_in_corpus) = double_time_prob(octave_errors_in_corpus);
figure();
bar([x, double_time_prob]);

correct_matches = intersect(likely_double_time, octave_errors_in_corpus);
recall = length(correct_matches) / length(octave_errors);
precision = length(correct_matches) / length(likely_double_time);
f_score = (2 * precision * recall) / (precision + recall);

% cast offs:

% cellfun(@name, find_patterns(corpus_patterns, [56, 62, 69, 100]), 'UniformOutput', false)
% cellfun(@plot_pattern, find_patterns(corpus_patterns, [56, 62, 69, 100]), 'UniformOutput', false)

end

