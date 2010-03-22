function [ f_score, precision, recall ] = double_time_test( root_dir, analysis_dir, sound_directory_root, annotated_tempi_file )
%double_time_test Tests known good beat tracking examples and a ground truth dataset of
%examples with octave errors. 
%   Computes a Weka ARFF file.
% double_time_test('~/Research/Data/IRCAM-Beat/RWC/', 'ForTempoAnalyses', '~/Local_Data/RWC_WAV', 'RWC_AnnotatedTempi.txt')
% double_time_test('~/Research/Data/IRCAM-Beat/RWC/', 'AllGoodBeatAnalyses', '~/Local_Data/RWC_WAV')
% double_time_test('~/Research/Data/IRCAM-Beat/EMI_TempoDiscrimination/', 'TempoAnalyses', '~/Research/Data/IRCAM-Beat/EMI_TempoDiscrimination/Audio', 'EMI_TempoAnnotatedFiles.txt');

absolute_analysis_dir = [tilde_expand(root_dir) analysis_dir];
reestimated_dir = [tilde_expand(root_dir) 'ReestimatedTempo'];

good_beat_tracking = make_dataset(absolute_analysis_dir, '.wav.markers.xml', 0);

% Exceptions
% 'RM-P009' Actually not double time, annotated wrongly.
% 'RM-P082' % Starts on correct tempo then doubles halfway through.

[double_time_probs, corpus_patterns] = double_time_of_corpus(good_beat_tracking, tilde_expand(sound_directory_root));

% The first column of the probabilities 
double_time_prob = double_time_probs(:,1);

% double_time_prob = cellfun(@double_time, corpus_patterns);
% mean(double_time_prob)
% std(double_time_prob)
above_mean = 0.5; % number of standard deviations above the mean.
threshold = mean(double_time_prob) + above_mean * std(double_time_prob)
% Since the double_time_prob is a ratio of half time quaver alternation
% against original time quaver alternation, it must be above 1.0 at a
% minimum for accepting as likely to be double time.
% threshold = 1.5;

likely_double_time = find(double_time_prob > threshold);
% likely_double_time = find((max(double_time_probs(:,2:3), [], 2) > 1.15) & (double_time_probs(:,1) < 1.0));
% Derived from Weka classification by regression:
% likely_double_time = find(round(0.1913 * double_time_probs(:,4) + 0.0247))

% The computed double time patterns.
double_time_patterns = corpus_patterns(likely_double_time);

double_time_filenames = cellfun(@name, double_time_patterns, 'UniformOutput', false);

% octave_error_patterns = pattern_for_corpus(octave_error_filenames, tilde_expand(sound_directory_root));
% cellfun(@name, octave_error_patterns, 'UniformOutput', false)

% The ground truth double time patterns.
octave_error_filenames = read_filenames([tilde_expand(root_dir) 'octave_errors.txt']);
octave_errors_in_corpus = find_patterns(corpus_patterns, octave_error_filenames);

ground_truth = zeros(size(corpus_patterns));
ground_truth(octave_errors_in_corpus) = 1; % double_time_prob(octave_errors_in_corpus);
figure();
bar([ground_truth * 4, double_time_prob]);

correct_matches = intersect(likely_double_time, octave_errors_in_corpus);
recall = length(correct_matches) / length(octave_error_filenames);
precision = length(correct_matches) / length(likely_double_time);
f_score = (2 * precision * recall) / (precision + recall);

write_corpus_as_arff('Double time from rhythm pattern', corpus_patterns, [tilde_expand(root_dir) 'Weka_Datasets/doubletime.arff'], double_time_probs, ground_truth);

% create the directory new, copy in the analyses and then reestimate,
% overwriting the double_time_patterns.
copyfile(absolute_analysis_dir, reestimated_dir);
reestimate_tempo(double_time_filenames, reestimated_dir, absolute_analysis_dir, sound_directory_root)

% If an annotated tempo file was not supplied, attempt to derive the evaluation from the annotated beat markers.
if (nargin < 4)
    evaluate_corpus_reestimation(absolute_analysis_dir, reestimated_dir, [tilde_expand(root_dir) 'Annotation']);
else
    evaluate_corpus_reestimation_tempo(absolute_analysis_dir, reestimated_dir, [tilde_expand(root_dir) annotated_tempi_file]);
end

fprintf('Data set %s size %d tracks\n', absolute_analysis_dir, length(good_beat_tracking));
fprintf('\nGround truth octave errors:\n');
cellfun(@disp, octave_error_filenames, 'UniformOutput', false);
fprintf('\nAssessed to be double time patterns:\n');
cellfun(@disp, double_time_filenames, 'UniformOutput', false);
fprintf('\nNumber correct %d, ground truth size %d, marked as double time %d\n',...
    length(correct_matches), length(octave_error_filenames), length(likely_double_time))
fprintf('Precision %.3f Recall %.3f F-Score %.3f\n', precision, recall, f_score);

end

