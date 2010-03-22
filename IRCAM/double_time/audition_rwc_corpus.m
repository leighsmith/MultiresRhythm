function audition_rwc_corpus( corpus_directory, annotation )
%audition_rwc_corpus Creates sound files mixing the original sound with the beat
%markers for all files in the corpus directory

corpus_files = dir([tilde_expand('~/Research/Data/IRCAM-Beat/RWC/') corpus_directory '/*.wav.markers.xml']);
maximum_examples = length(corpus_files);

song_index = 1;
while (song_index <= maximum_examples)
    corpus_pathname = corpus_files(song_index).name;
    % Remove all hidden files.
    if (corpus_pathname(1) ~= '.' && ~corpus_files(song_index).isdir && ~strcmp(corpus_pathname, 'README.TXT'))
        filename = basename(corpus_pathname);
        if annotation
            audition_rwc_file(sprintf('~/Research/Data/IRCAM-Beat/RWC/Annotation/%s.beat.xml', filename), 'annotated');
        else
            audition_rwc_file(sprintf('~/Research/Data/IRCAM-Beat/RWC/%s/%s.wav.markers.xml', corpus_directory, filename), 'tracked'));
        end
    end

    song_index = song_index + 1;
end



end

