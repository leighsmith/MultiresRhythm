function [ new_rhythm_description ] = read_rhythm_description( beat_markers_filepath, audio_filepath, subband_ranges )
%read_rhythm_description Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter assigned from the ircambeat computed values.
% $Id$

% Assign the wideband_odf as the entire spectral energy.
if(nargin < 3)
    [wideband_odf, sample_rate, odf_subbands] = odf_of_file(audio_filepath);
else
    [wideband_odf, sample_rate, odf_subbands] = odf_of_file(audio_filepath, subband_ranges);
end

% bpm_filepath = [beat_markers_filepath '.wav.bpm.xml'];
% [beat_times, beat_markers] = read_ircam_marker_times(beat_markers_filepath);
[beat_times, beat_markers] = read_beats(beat_markers_filepath);
% fprintf('beat markers %s beat times %s\n', sprintf('%d ', beat_markers(1:10)), sprintf('%f ', beat_times(1:10)))

downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat

[all_beats_per_measure, metrical_hierarchy] = read_ircam_timesignatures(beat_markers_filepath);
beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

% calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
tempo = 60 / median(diff(beat_times)); 

% Supply wideband_odfs and frequency specific bands as the same for now.
% Should eventually handle when there are no subband odfs.
[filepath, filename] = fileparts(audio_filepath);
new_rhythm_description = RhythmDescription(filename, wideband_odf, odf_subbands, ...
    beat_times, beat_markers, metrical_hierarchy, beats_per_measure, anacrusis, ...
    sample_rate, tempo);

end
