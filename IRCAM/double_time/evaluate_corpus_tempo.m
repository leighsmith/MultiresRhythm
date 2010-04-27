function [ song_scores, octave_errors ] = evaluate_corpus_tempo( analysis_dir, annotated_tempo_file )
%evaluate_corpus_tempo Returns the scores of tempo matching to the annotated tempi.
%   Returns those files which are octave errors (according to tempo, not beat tracking).

analysed_files = make_dataset(tilde_expand(analysis_dir), '.wav.markers.xml', 0);

fid = fopen(tilde_expand(annotated_tempo_file), 'r');
names_and_tempi = textscan(fid, '%q %f');
annotated_files = names_and_tempi{1};
annotated_tempi = names_and_tempi{2};
fclose(fid);
octave_errors = cell(1, 2);
octave_error_index = 1;

tempo_threshold = 0.03; % percentage difference from annotated tempo required to past muster.
dataset_size = length(analysed_files);
song_scores = zeros(1, dataset_size);

for i = 1 : dataset_size
    % retrieve the annotated tempi from the analysed file name.
    tempo_index = strmatch(basename(analysed_files{i}), annotated_files);
    annotated_tempo = annotated_tempi(tempo_index);
    beatMarkerTimes = read_quaero_beat_markers(analysed_files{i});
    tracked_tempo = 60 / median(diff(beatMarkerTimes));

    fprintf('%3d: %s annotated tempo %.3f tracked tempo %.3f\n', i, basename(analysed_files{i}), annotated_tempo, tracked_tempo);
    tempo_difference = abs(annotated_tempo - tracked_tempo) / annotated_tempo;
    song_scores(i) = tempo_difference < tempo_threshold;
    if(~song_scores(i))
        fprintf('mismatched tempo differs by %.3f\n', tempo_difference);
        % Check if the tempo mismatch is actually an octave error.
        octave = log2(tracked_tempo / annotated_tempo);
        if(abs(round(octave) - octave) < tempo_threshold)
            fprintf('but likely octave error\n');
            octave_errors{octave_error_index} = basename(analysed_files{i});
            octave_error_index = octave_error_index + 1;
        end
    end

end

end

