function [ similarity_matrix, corpus_patterns ] = rhythm_similarity_of_annotated_corpus(corpus_name)
%rhythm_similarity_of_annotated_corpus Returns the similarity matrix of the corpus named by corpus_name.
% for example: rhythm_similarity_of_annotated_corpus('Quaero_Selection');
% $Id$

% The directories we work on.
annotations_directory = tilde_expand(['~/Research/Data/IRCAM-Beat/' corpus_name '/Annotation/']);
sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';
beat_file_extension = '.beat.xml';

[ similarity_matrix, corpus_patterns ] = rhythm_similarity_of_corpus(corpus_name, annotations_directory, beat_file_extension, sound_directory_root);

end
