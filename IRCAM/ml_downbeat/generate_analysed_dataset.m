function [corpus_patterns] = generate_analysed_dataset(analysis_directory, corpus_directory, sound_directory_root)
%generate_analysed_dataset Reads the dataset and creates an ARFF file for Weka.
%   Computes the anacruses by counting the computed beats until the first
%   annotated downbeat. That is we assume there are no downbeats marked
%   during analysis by ircambeat.
%
% e.g generate_analysed_dataset('~/Research/Data/IRCAM-Beat/Quaero_Selection/','~/Research/Data/IRCAM-Beat/Quaero_Selection/Audio')

% Produce a cell array of analysed files and the annotated anacrusis.
fprintf('Gathering ground truth anacruses\n');
[corpus, anacruses] = ground_truth_anacruses(analysis_directory, corpus_directory);

fprintf('Gathering patterns\n');
corpus_patterns = pattern_for_corpus(corpus, tilde_expand(sound_directory_root));

% Assign the patterns with the ground truth anacrusis.
for i = 1 : length(anacruses)
    corpus_patterns{i}.anacrusis = anacruses(i);
end

arff_file = tilde_expand([analysis_directory '/downbeats.arff']);
fprintf('Writing ARFF file to %s\n', arff_file);
write_corpus_as_arff(corpus_patterns, arff_file);

end
