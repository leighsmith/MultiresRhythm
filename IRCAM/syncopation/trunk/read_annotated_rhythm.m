function [ annotated_rhythm ] = read_annotated_rhythm( filename )
%read_annotated_rhythm Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter.
% $Id$

rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

odf_filepath = [rhythm_directory_root 'Analysis/' filename '.odf'];
sample_rate = 172.27; % hardwired, again.
odf = ircam_odf(odf_filepath);

annotated_beat_markers_filepath = [rhythm_directory_root 'Annotation/' filename '.b.xml'];
[beat_times, beat_markers] = annotated_beats(annotated_beat_markers_filepath);
start_from = beat_times(1);
start_sample = round(sample_rate * start_from) + 1;
odf_subset = odf(start_sample : end);

all_beats_per_measure = read_ircam_annotation_timesignatures(annotated_beat_markers_filepath);
beats_per_measure = all_beats_per_measure(1); % TODO take the first.

meter = [2 2 2 2]; % TODO hardwired!

annotated_rhythm = RhythmDescription(filename, odf_subset, beat_times, meter, beats_per_measure, sample_rate);
end
