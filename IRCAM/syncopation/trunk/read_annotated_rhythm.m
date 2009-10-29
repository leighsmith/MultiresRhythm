function [ annotated_rhythm ] = read_annotated_rhythm( filename, rhythm_directory_root, annotation_source )
%read_annotated_rhythm Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter.
% $Id$

if (nargin < 2)
    rhythm_directory_root = '~/Research/Data/IRCAM-Beat/Quaero_Selection/';
end

abs_rhythm_directory = tilde_expand(rhythm_directory_root);
%odf_filepath = [abs_rhythm_directory 'Analysis/' filename '.odf'];
%sample_rate = 172.27; % hardwired, again.
%odf = ircam_odf(odf_filepath);
[computed_odf, sample_rate, odf_subbands] = odf([sound_directory_root filename '.wav']);

annotated_beat_markers_filepath = [abs_rhythm_directory 'Annotation/' filename annotation_source '.xml'];
[beat_times, beat_markers] = annotated_beats(annotated_beat_markers_filepath);
% fprintf('beat markers %s beat times %s\n', sprintf('%d ', beat_markers(1:10)), sprintf('%f ', beat_times(1:10)))
downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat
start_from = beat_times(downbeats(1));
start_sample = round(sample_rate * start_from) + 1;
odf_subset = computed_odf(start_sample : end);
odf_subbands_subset = odf_subbands(:, start_sample : end);

[all_beats_per_measure, metrical_hierarchy] = read_ircam_annotation_timesignatures(annotated_beat_markers_filepath);
beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

tempo = 60/median(diff(beat_times)); % calculate from beat times. Perhaps one day read it from XML file.
annotated_rhythm = RhythmDescription(filename, odf_subset, odf_subbands_subset, beat_times, metrical_hierarchy, beats_per_measure, anacrusis, sample_rate, tempo);
end
