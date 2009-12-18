function [ new_rhythm_description ] = read_rhythm_description( beat_markers_filepath, audio_filepath )
%read_rhythm_description Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter assigned from the ircambeat computed values.
% $Id$

[computed_odf, sample_rate, odf_subbands] = odf(audio_filepath);

% bpm_filepath = [beat_markers_filepath '.wav.bpm.xml'];
% [beat_times, beat_markers] = read_ircam_marker_times(beat_markers_filepath);
[beat_times, beat_markers] = read_beats(beat_markers_filepath);
% fprintf('beat markers %s beat times %s\n', sprintf('%d ', beat_markers(1:10)), sprintf('%f ', beat_times(1:10)))

downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat
% start_from = beat_times(downbeats(1));
% start_sample = round(sample_rate * start_from) + 1;

% The wideband_odf uses the entire spectral energy.
% TODO LMS commented out so we use the full ODF for matching
% wideband_odf = computed_odf(start_sample : end);
wideband_odf = computed_odf;
% Create an matrix of different subbands.
% odfs = odf_subbands(:, start_sample : end);

[all_beats_per_measure, metrical_hierarchy] = read_ircam_annotation_timesignatures(beat_markers_filepath);
beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

% calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
tempo = 60/median(diff(beat_times)); 

% Disabled while we are not trimming the ODF according to downbeat locations.
% fprintf('Starting downbeat finding from %.3f seconds, %.3f samples\n', start_from, start_sample);

% Supply wideband_odfs and frequency specific bands as the same for now.
% Should eventually handle when there are no subband odfs.
[filepath, filename, extension, version] = fileparts(audio_filepath);
new_rhythm_description = RhythmDescription(filename, wideband_odf, odf_subbands, beat_times, metrical_hierarchy, beats_per_measure, anacrusis, sample_rate, tempo);

end
