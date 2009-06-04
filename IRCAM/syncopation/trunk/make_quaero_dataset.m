function [ dataset ] = make_quaero_dataset( maximum_examples, every, annotations_directory )
%make_quaero_dataset Returns a cell vector of names of files in the corpus,
%stripped of extensions.
%   Detailed explanation goes here

start_at = 1;

% make_quaero_dataset('/Users/lsmith/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation')
annotation_files = dir(annotations_directory);
dataset = cell(1, maximum_examples);

song_index = start_at;
dataset_index = 1;
while (dataset_index <= maximum_examples)
     annotation_pathname = annotation_files(song_index).name;
     if (annotation_pathname(1) ~= '.') % Remove all hidden files.
        annotation_name = strtok(annotation_pathname, '.');
        % ircambeat_marker_pathname = (beat_marker_filepath_anno annotation_pathname)
        % beats_per_measure = (mapcar #'first (read_ircam_annotation_timesignatures annotation_pathname))
        %% when (cl_fad:file_exists_p ircambeat_marker_pathname) % TODO this is rather useless here.
        %% if we want to exclude pieces with long preceding non_metrical intervals.
        %% when (< anacrusis beats_per_measure) 
        % when (equal beats_per_measure (list only_measures_of_beats))      % TODO exclude non_strict 4/4 for now.

        %% Get the initial upbeat
        % (destructuring_bind (anacrusis downbeat_time)
	    % (annotated_anacrusis annotation_pathname ircambeat_marker_pathname (first beats_per_measure))
        % fprintf('~a anacrusis ~d, downbeat time ~,3f beats_per_measure ~a~%'
		% annotation_name, anacrusis, downbeat_time, beats_per_measure)
	    dataset{dataset_index} = annotation_name;
        % (list annotation_name :anacrusis anacrusis :annotation_filepath
        % annotation_pathname :first_downbeat_time downbeat_time))))
        dataset_index = dataset_index + 1;
     end

     song_index = song_index + every;
      
end

