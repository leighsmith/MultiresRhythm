function [ dataset_for_meter ] = make_dataset( annotations_directory, beat_file_extension, only_measures_of_beats )
%make_dataset Returns a cell vector of absolute pathnames of files in the corpus.
% $Id: make_quaero_dataset.m 5573 2010-01-05 15:09:03Z leighsmi $

start_at = 1;
if (nargin < 3)
    only_measures_of_beats = 4;       % TODO exclude non_strict 4/4 for now.
end

annotation_files = dir([annotations_directory '/*' beat_file_extension]);
maximum_examples = length(annotation_files);
dataset = cell(1, maximum_examples);

song_index = start_at;
dataset_index = 1;
while (dataset_index <= maximum_examples && song_index <= maximum_examples)
     annotation_pathname = annotation_files(song_index).name;
     if (annotation_pathname(1) ~= '.' && ~annotation_files(song_index).isdir) % Remove all hidden files.
        beat_marker_filepath = [annotations_directory '/' annotation_pathname];
        % ircambeat_marker_pathname = (beat_marker_filepath_anno annotation_pathname)
        beats_per_measure = read_ircam_timesignatures(beat_marker_filepath);

        %% if we want to exclude pieces with long preceding non_metrical intervals.
        %% when (< anacrusis beats_per_measure)
        % We exclude all non-strictly single meters
        if(only_measures_of_beats ~= 0 && mean(beats_per_measure(:, 1)) == only_measures_of_beats)
            %% Get the initial upbeat
            % [anacrusis downbeat_time] = annotated_anacrusis(annotation_pathname ircambeat_marker_pathname (first beats_per_measure))
            % fprintf('%a anacrusis %d, downbeat time %,3f beats_per_measure %a\n', annotation_name, anacrusis, downbeat_time, beats_per_measure)
            dataset{dataset_index} = beat_marker_filepath;
            % (list annotation_name :anacrusis anacrusis :annotation_filepath
            % annotation_pathname :first_downbeat_time downbeat_time))))
            dataset_index = dataset_index + 1;
        else
            fprintf('excluded %s\n', annotation_pathname);
        end
     end

     song_index = song_index + 1;
end

dataset_for_meter = dataset(1 : dataset_index - 1);

end

