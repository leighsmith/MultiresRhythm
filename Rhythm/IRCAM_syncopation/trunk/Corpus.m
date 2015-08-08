classdef Corpus
    
%Corpus Holds the corpus to be analysed.
%
% $Id$
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.
% 

properties
    name = '';
    song_names = {};
    directory_root = '';
end
    
methods
        
function new_corpus = Corpus( corpus_name, corpus_directory )
%Corpus Returns an instance of Corpus with the song_names cell vector
%property populated with names of files in the corpus, stripped of extensions.

annotations_directory = [corpus_directory '/Annotation'];
annotation_files = dir(annotations_directory);
dataset = cell(1, length(annotation_files));

song_index = 1;
dataset_index = 1;
while ((dataset_index <= length(annotation_files)) && (song_index <= length(annotation_files)))
     annotation_pathname = annotation_files(song_index).name;
     if (annotation_pathname(1) ~= '.') % Remove all hidden files.
        dataset{dataset_index} = strtok(annotation_pathname, '.');
        dataset_index = dataset_index + 1;
     end

     song_index = song_index + 1;
end

new_corpus.name = corpus_name;
new_corpus.song_names = dataset(1 : dataset_index - 1);
new_corpus.directory_root = tilde_expand(corpus_directory);

end % Corpus factory method.

function filename = fileNameOfCorpusItem(corpus, index)
    filename = [directory_root song_names{index}];
end

end % methods
    
end

