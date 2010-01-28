function [ ground_truth ] = ground_truth_anacruses( computed_dir )
%ground_truth_anacruses Prints the anacruses for each piece 
%   Computed by finding the minimum distance between the annotated downbeats and the
%computed versions.

annotated_downbeat_files = prune_bad_beat_tracking(computed_dir, ...
        make_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation/'), '.beat.xml'));

dataset_size = length(annotated_downbeat_files);
ground_truth = cell(1, dataset_size);

for i = 1 : dataset_size
    [directory, filename] = fileparts(annotated_downbeat_files{i});
    piece_name = strtok(filename, '.');
    computed_downbeat_file = tilde_expand(['~/Research/Data/IRCAM-Beat/Quaero_Selection/' computed_dir '/' piece_name '.wav.markers.xml']);

    % piece_ground_truth = struct('anacrusis', 0, 'downbeat_file', '', 'annotation_file', '');
    ground_truth{i}.anacrusis = find_anacrusis(annotated_downbeat_files{i}, computed_downbeat_file);
    ground_truth{i}.downbeat_file = computed_downbeat_file;
    ground_truth{i}.annotation_file = annotated_downbeat_files{i};
    % Compute the minimum distance using the selected anacrusis.
    min_distance = downbeat_distances(annotated_downbeat_files{i}, computed_downbeat_file, ground_truth{i}.anacrusis);
    fprintf('%2d: %s anacrusis: %d, min distance: %.3f\n', i, piece_name, ground_truth{i}.anacrusis, min_distance);
end

end

