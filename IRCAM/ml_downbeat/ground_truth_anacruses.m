function [ computed_downbeat_files, anacruses ] = ground_truth_anacruses( computed_dir, annotated_dir )
%ground_truth_anacruses Returns the anacruses for each piece.
%   Computed by finding the minimum distance between the annotated downbeats and the
%computed versions.

annotated_downbeat_files = make_dataset(tilde_expand(annotated_dir), '.beat.xml');

% We return the computed files if they match the annotations and the
% matching indices from annotated_downbeat_files.
[computed_downbeat_files, annotation_indices] = prune_bad_beat_tracking(computed_dir, annotated_downbeat_files);

dataset_size = length(computed_downbeat_files);
anacruses = zeros(1, dataset_size);

for i = 1 : dataset_size
    matching_annotation_file = annotated_downbeat_files{annotation_indices(i)};
    % Return the number of beats computed by ircambeat before the first true (annotated) downbeat time.
    anacruses(i) = find_anacrusis(matching_annotation_file, computed_downbeat_files{i});

    % Compute the minimum distance using the computed anacrusis to verify.
    min_distance = downbeat_distances(matching_annotation_file, computed_downbeat_files{i}, anacruses(i));
    fprintf('%2d: %s anacrusis: %d, min distance: %.3f\n', i, computed_downbeat_files{i}, anacruses(i), min_distance);
end

end

