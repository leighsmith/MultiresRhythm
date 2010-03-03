function [ f_score ] = downbeat_accuracy( computed_dir )
%downbeat_accuracy Returns the percentage of analyses that match downbeats of the annotations.
%   Precision determines how close the beats must be to qualify as correctly matched.
% $Id$
%

% Probably should loop across precision ranges for area under the curve.
precision = 0.100; % In milliseconds

annotated_downbeat_files = make_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation/'), '.beat.xml');
dataset_size = length(annotated_downbeat_files);
distance = zeros(1, dataset_size);

for i = 1 : dataset_size
    piece_name = basename(annotated_downbeat_files{i});
    computed_downbeat_file = tilde_expand(['~/Research/Data/IRCAM-Beat/Quaero_Selection/' computed_dir '/' piece_name '.wav.markers.xml']);
    fprintf('%d: %s\n', i, piece_name);
    distance(i) = downbeat_distances(annotated_downbeat_files{i}, computed_downbeat_file);
end

score = sum(distance < precision);
f_score = score / dataset_size;

figure();
bar(distance);
title(sprintf('Downbeat errors for %s %d correct F-Score %4.3f', computed_dir, score, f_score));

end


