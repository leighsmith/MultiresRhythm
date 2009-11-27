function [ analysed_rhythm ] = read_analysed_rhythm( beat_markers_filepath, audio_filepath )
%read_annotated_rhythm Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter assigned from the ircambeat computed values.
% $Id$

[computed_odf, sample_rate, odf_subbands] = odf(audio_filepath);

% bpm_filepath = [abs_rhythm_directory 'Analysis/' filename '.wav.bpm.xml'];
[beat_times, beat_markers] = read_ircam_marker_times(beat_markers_filepath);
% Compare how these differ.
% [beat_times, beat_markers] = annotated_beats(annotated_beat_markers_filepath);

downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat
start_from = beat_times(downbeats(1));
start_sample = round(sample_rate * start_from) + 1;

% TODO LMS commented out so we use the full ODF for matching
% wideband_odf = odf(start_sample : end);
wideband_odf = computed_odf;

beats_per_measure = 4; % TODO hardwired! Need to read from .wav.markers.xml file
meter = [2 2 2 2]; % TODO hardwired!
% [all_beats_per_measure, metrical_hierarchy] = read_ircam_annotation_timesignatures(annotated_beat_markers_filepath);
% beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

% calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
tempo = 60/median(diff(beat_times)); 

fprintf('Starting downbeat finding from %.3f seconds, %.3f samples\n', start_from, start_sample);

% Supply wideband_odfs and frequency specific bands as the same for now.
% Should eventually handle when there are no subband odfs.
[filepath, filename, extension, version] = fileparts(audio_filepath);
analysed_rhythm = RhythmDescription(filename, wideband_odf, odf_subbands, beat_times, meter, beats_per_measure, anacrusis, sample_rate, tempo);
end
