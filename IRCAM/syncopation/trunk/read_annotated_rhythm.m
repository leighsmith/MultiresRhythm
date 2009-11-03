function [ annotated_rhythm ] = read_annotated_rhythm( annotated_beat_markers_filepath, audio_filepath )
%read_annotated_rhythm Returns an instance of RhythmDescription with the onset detection functions, times of
%each beat, beats per measure, downbeat times, meter.
% $Id$

[computed_odf, sample_rate, odf_subbands] = odf(audio_filepath);

[beat_times, beat_markers] = annotated_beats(annotated_beat_markers_filepath);
% fprintf('beat markers %s beat times %s\n', sprintf('%d ', beat_markers(1:10)), sprintf('%f ', beat_times(1:10)))
downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat
start_from = beat_times(downbeats(1));
start_sample = round(sample_rate * start_from) + 1;
% Create an matrix of different subbands. 
% The entire spectral energy is the first row.
% odfs = [computed_odf(start_sample : end)'; odf_subbands(:, start_sample : end)];
% Alternative is to only consider the subbands
odfs = odf_subbands(:, start_sample : end);

[all_beats_per_measure, metrical_hierarchy] = read_ircam_annotation_timesignatures(annotated_beat_markers_filepath);
beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

tempo = 60 / median(diff(beat_times)); % calculate from beat times. Perhaps one day read it from XML file.
[filepath, filename, extension, version] = fileparts(audio_filepath);
annotated_rhythm = RhythmDescription(filename, odfs, beat_times, metrical_hierarchy, beats_per_measure, anacrusis, sample_rate, tempo);

end
