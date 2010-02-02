function [ acceptable_files, annotation_indices ] = prune_bad_beat_tracking( computed_dir, annotated_downbeat_files )
%prune_bad_beat_tracking Returns the set of computed downbeat files
%that have good beat tracking matches to the annotated downbeat files.
% Returns the indices of the annotated downbeat files that are used.

f_score_threshold = 0.5; % performance required to past muster.
dataset_size = length(annotated_downbeat_files);
% We preallocate it, but we expect it to grow, since trying to resize a
% cell array is so painful.
acceptable_files = cell(1, 10);
annotation_indices = zeros(1, 10);
write_index = 1;

for i = 1 : dataset_size
    [directory, filename] = fileparts(annotated_downbeat_files{i});
    piece_name = strtok(filename, '.');
    computed_downbeat_file = tilde_expand([computed_dir '/' piece_name '.wav.markers.xml']);

    beats_per_measure = read_ircam_timesignatures(computed_downbeat_file);
    if(beats_per_measure ~= 4)
        fprintf('%s analysis not 4/4\n', piece_name);
    else
        % Evaluate how well the beats match the annotations.
        evaluation = Evaluation();
        % evaluation.annotationFilter = @downbeats_filter;
        evaluation.annotationFilter = ''; % Check accuracy of all beats.
        evaluation.relativePrecision = true;
        evaluation.precisionWindow = 0.15; % Within 15% of the annotated beat position.
        [precision, recall, f_score] = evaluate_beat_file(computed_downbeat_file, annotated_downbeat_files{i}, evaluation);

        fprintf('%2d: %s f_score %.3f\n', i, piece_name, f_score);
        if (f_score > f_score_threshold)
            acceptable_files{write_index} = computed_downbeat_file;
            annotation_indices(write_index) = i;
            write_index = write_index + 1;
        else
            fprintf('Low F-score, rejecting\n');
        end
    end
end

end

