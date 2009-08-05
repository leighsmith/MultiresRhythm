function [ dataset_for_meter ] = make_quaero_dataset( maximum_examples, every, quaero_directory )
%make_quaero_dataset Returns a cell vector of names of files in the corpus,
%stripped of extensions.
% $Id$

start_at = 1;
only_measures_of_beats = 4;       % TODO exclude non_strict 4/4 for now.

annotations_directory = tilde_expand(['~/Research/Data/IRCAM-Beat/' quaero_directory '/Annotation']);
annotation_files = dir(annotations_directory);
dataset = cell(1, maximum_examples);

song_index = start_at;
dataset_index = 1;
while (dataset_index <= maximum_examples & song_index <= length(annotation_files))
     annotation_pathname = annotation_files(song_index).name;
     if (annotation_pathname(1) ~= '.') % Remove all hidden files.
        annotation_name = strtok(annotation_pathname, '.');
        % ircambeat_marker_pathname = (beat_marker_filepath_anno annotation_pathname)
        beats_per_measure = read_ircam_annotation_timesignatures([annotations_directory '/' annotation_pathname], 'marker');

        %% if we want to exclude pieces with long preceding non_metrical intervals.
        %% when (< anacrusis beats_per_measure)
        % We exclude all non-strictly single meters
        if(mean(beats_per_measure(:, 1)) == only_measures_of_beats)
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
     end

     song_index = song_index + every;
end

dataset_for_meter = dataset(1 : dataset_index - 1);

end

