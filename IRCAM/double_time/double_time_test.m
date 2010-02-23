function [ f_score, precision, recall ] = double_time_test( analysis_dir )
%double_time_test Tests known good beat tracking examples and a ground truth dataset of
%examples with octave errors. 
%   Computes a Weka ARFF file.
% double_time_test('GoodBeatAnalysis')

good_beat_tracking = make_dataset(tilde_expand(['~/Research/Data/IRCAM-Beat/RWC/' analysis_dir]), '.wav.markers.xml');
sound_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/RWC/Audio/WAV');

octave_error_filenames = cell(1, 5);

% These are the double time examples.
% 'RM-P009',... % Actually not double time, annotated wrongly.
% 'RM-P082',... % Starts on correct tempo then doubles halfway through.

octave_errors_fid = fopen(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/octave_errors.txt'), 'r');
line_index = 1;
while ~feof(octave_errors_fid)
    octave_error_filenames{line_index} = fgetl(octave_errors_fid);
    line_index = line_index + 1;
end
fclose(octave_errors_fid);

% quaver_alternation_prob = 1 - cellfun(@quaver_alternation, corpus_patterns);
% freq_alternation_prob = 1 - cellfun(@frequency_alternation, corpus_patterns);
% double_time_prob = quaver_alternation_prob;
% double_time_prob = freq_alternation_prob;
% double_time_prob = freq_alternation_prob .* quaver_alternation_prob;
% double_time_prob = freq_alternation_prob + quaver_alternation_prob;
% threshold = mean(double_time_prob) + std(double_time_prob);

[double_time_probs, corpus_patterns] = double_time_of_corpus(good_beat_tracking, sound_directory_root);

% double_time_prob = double_time_probs(:, 4);
double_time_prob = double_time_probs(:,2) ./ double_time_probs(:,1);
% double_time_prob = min(double_time_probs(:,2:3), [], 2) ./ double_time_probs(:,1);
% double_time_prob = max(double_time_probs(:,2:3), [], 2) ./ double_time_probs(:,1);
% double_time_prob = (double_time_probs(:,2) + double_time_probs(:,3)) ./ (2 * double_time_probs(:,1));

% double_time_prob = cellfun(@double_time, corpus_patterns);
% mean(double_time_prob)
% std(double_time_prob)
above_mean = 0.0; % number of standard deviations above the mean.
threshold = mean(double_time_prob) + above_mean * std(double_time_prob);
% Since the double_time_prob is a ratio of half time quaver alternation
% against original time quaver alternation, it must be above 1.0 at a
% minimum for accepting as likely to be double time.
% threshold = 1.5;

likely_double_time = find(double_time_prob > threshold);
% likely_double_time = find((max(double_time_probs(:,2:3), [], 2) > 1.15) & (double_time_probs(:,1) < 1.0));
% Derived from Weka classification by regression:
% likely_double_time = find(round(0.1913 * double_time_probs(:,4) + 0.0247))

double_time_patterns = corpus_patterns(likely_double_time);

cellfun(@name, double_time_patterns, 'UniformOutput', false)
% octave_error_patterns = pattern_for_corpus(octave_error_filenames, tilde_expand(sound_directory_root));
% cellfun(@name, octave_error_patterns, 'UniformOutput', false)

octave_errors_in_corpus = find_patterns(corpus_patterns, octave_error_filenames);

ground_truth = zeros(size(corpus_patterns));
ground_truth(octave_errors_in_corpus) = 1; % double_time_prob(octave_errors_in_corpus);
figure();
bar([ground_truth * 4, double_time_prob]);

correct_matches = intersect(likely_double_time, octave_errors_in_corpus);
fprintf('Number correct %d, ground truth size %d, marked as double time %d\n',...
    length(correct_matches), length(octave_error_filenames), length(likely_double_time))
recall = length(correct_matches) / length(octave_error_filenames);
precision = length(correct_matches) / length(likely_double_time);
f_score = (2 * precision * recall) / (precision + recall);

write_corpus_as_arff('Double time from rhythm pattern', corpus_patterns, '~/Research/Data/IRCAM-Beat/RWC/doubletime.arff', double_time_probs, ground_truth);

end

