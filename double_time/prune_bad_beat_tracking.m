function [ acceptable_files, octave_errors, annotation_indices ] = prune_bad_beat_tracking( computed_dir, annotated_beat_files, only_beats )
%prune_bad_beat_tracking Returns the set of computed downbeat files
%that have good beat tracking matches to the annotated downbeat files.
% Returns the indices of the annotated downbeat files that are used.

f_score_threshold = 0.5; % performance required to past muster.
dataset_size = length(annotated_beat_files);
% We preallocate it, but we expect it to grow, since trying to resize a
% cell array is so painful.
acceptable_files = cell(1, 5);
annotation_indices = zeros(1, 5);
octave_errors = cell(1, 2);
octave_error_index = 1;
write_index = 1;

for i = 1 : dataset_size
    piece_name = basename(annotated_beat_files{i});
    computed_downbeat_file = tilde_expand([computed_dir '/' piece_name '.wav.markers.xml']);

    beats_per_measure = read_ircam_timesignatures(computed_downbeat_file);
    if(nargin >= 3 && beats_per_measure ~= only_beats)
        fprintf('%s analysis not %d/4\n', piece_name, only_beats);
    else
        % Evaluate how well the beats match the annotations.
        evaluation = Evaluation();
        % evaluation.annotationFilter = @downbeats_filter;
        evaluation.annotationFilter = ''; % Check accuracy of all beats.
        % evaluation.annotationFilter = @halfbeats_filter; % Check accuracy of all beats.
        evaluation.relativePrecision = true;
        evaluation.precisionWindow = 0.15; % Within 15% of the annotated beat position.
        [precision, recall, f_score] = evaluate_beat_file(computed_downbeat_file, annotated_beat_files{i}, evaluation);

        fprintf('%2d: %s f_score %.3f precision %.3f recall %.3f\n', i, piece_name, f_score, precision, recall);
        if (f_score > f_score_threshold)
            acceptable_file = true;
            if(round(recall / precision) == 2)
                fprintf('Possible octave error\n');
                [annotationTimes, annotationMarkers] = annotated_beats(annotated_beat_files{i});
                if(max(diff(annotationMarkers)) > 1)
                    fprintf('not all beats annotated\n');
                    acceptable_file = false;
                else
                    octave_errors{octave_error_index} = computed_downbeat_file;
                    octave_error_index = octave_error_index + 1;
                end
            end
            if (acceptable_file)
                acceptable_files{write_index} = computed_downbeat_file;
                annotation_indices(write_index) = i;
                write_index = write_index + 1;
            end
        else
            fprintf('Low F-score, rejecting\n');
        end
        fprintf('\n');
    end
end

end

