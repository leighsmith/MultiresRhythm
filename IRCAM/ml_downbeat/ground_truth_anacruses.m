function [ anacrusis ] = ground_truth_anacruses( computed_dir )
%ground_truth_anacruses Summary of this function goes here
%   Detailed explanation goes here

precision = 0.100; % In milliseconds

annotated_downbeat_files = make_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation/'), '.beat.xml');
dataset_size = length(annotated_downbeat_files);
anacrusis = zeros(1, dataset_size);

for i = 1 : dataset_size
    [directory, filename] = fileparts(annotated_downbeat_files{i});
    piece_name = strtok(filename, '.');
    computed_downbeat_file = tilde_expand(['~/Research/Data/IRCAM-Beat/Quaero_Selection/' computed_dir '/' piece_name '.wav.markers.xml']);
    anacrusis(i) = find_anacrusis(annotated_downbeat_files{i}, computed_downbeat_file);
    fprintf('%d: %s %d\n', i, piece_name, anacrusis(i));
end

end

