function [corpus_patterns] = generate_annotated_dataset( corpus_directory, sound_directory_root )
%annotated_anacruses_dataset Reads the dataset and creates an ARFF file for Weka.
%   Retrieves the anacruses from the downbeats in the beat file, that is,
%   we assume the downbeats have been annotated.
%
%   generate_annotated_dataset('~/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation/')
%   generate_annotated_dataset('~/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/')
annotated_downbeat_files = make_dataset(tilde_expand(corpus_directory), '.beat.xml');
fprintf('Gathering patterns\n');
corpus_patterns = pattern_for_corpus(annotated_downbeat_files, tilde_expand(sound_directory_root));

arff_file = tilde_expand([corpus_directory '/downbeats.arff']);
fprintf('Writing ARFF file to %s\n', arff_file);
write_corpus_as_arff(corpus_patterns, arff_file);

end

