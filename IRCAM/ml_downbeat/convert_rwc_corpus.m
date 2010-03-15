function convert_rwc_corpus( directory )
%convert_rwc_corpus Convert all files in directory from RWC format to IRCAM beat format.
%   convert_rwc_corpus('AIST.RWC-MDB-P-2001.BEAT')

rwc_dir = tilde_expand(['~/Research/Data/IRCAM-Beat/RWC/Annotation_orig/' directory '.TXT/']);
ircam_dir = tilde_expand(['~/Research/Data/IRCAM-Beat/RWC/Annotation_orig/' directory '/']);

beat_file_extension = '.TXT';

annotation_files = dir([rwc_dir '/*' beat_file_extension]);
maximum_examples = length(annotation_files);

song_index = 1;
while (song_index <= maximum_examples)
     annotation_pathname = annotation_files(song_index).name;
     if (annotation_pathname(1) ~= '.' && ~annotation_files(song_index).isdir && ~strcmp(annotation_pathname, 'README.TXT')) % Remove all hidden files.
        beat_marker_filepath = [rwc_dir '/' annotation_pathname];
        filename = strtok(annotation_pathname,'.');
        
        fprintf('%s %s\n', beat_marker_filepath, [ircam_dir filename '.beat.xml']);
        rwc_to_ircam(beat_marker_filepath, [ircam_dir filename '.beat.xml']);
     end

     song_index = song_index + 1;
end


end

