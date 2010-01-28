function generate_dataset()
%generate_dataset Reads the dataset and creates an ARFF file for Weka.
%   Detailed explanation goes here

sound_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/Audio');
% Produce a cell array of analysed files and the annotated anacrusis.
fprintf('Gathering ground truth anacruses\n');
anacruses = ground_truth_anacruses('Analysis');
corpus = cellfun(@downbeat_filename, anacruses, 'UniformOutput', false);
fprintf('Gathering patterns\n');
corpus_patterns = pattern_for_corpus(corpus, sound_directory_root);

for i = 1 : length(anacruses)
    corpus_patterns{i}.anacrusis = anacruses{i}.anacrusis;
end
fprintf('Writing ARFF file\n');
write_corpus_as_arff(corpus_patterns, tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/downbeats.arff'));

end

function [filename] = downbeat_filename (structure)
    filename = structure.downbeat_file;
end