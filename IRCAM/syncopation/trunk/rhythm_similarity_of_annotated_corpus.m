function [ similarity_matrix, corpus_patterns ] = rhythm_similarity_of_annotated_corpus(corpus_name)
%rhythm_similarity_of_corpus Returns the similarity matrix of the corpus named by corpus_name.
% for example: corpus_similarity('Quaero_Selection');
% $Id: rhythm_similarity_of_corpus.m 5567 2009-12-21 15:13:24Z leighsmi $

% The directories we work on.
annotations_directory = tilde_expand(['~/Research/Data/IRCAM-Beat/' corpus_name '/Annotation/']);
sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';
beat_file_extension = '.beat.xml';

[ similarity_matrix, corpus_patterns ] = rhythm_similarity_of_corpus(annotations_directory, beat_file_extension, sound_directory_root);

end
