function [ analysed_rhythm ] = read_analysed_rhythm( filename, rhythm_directory_root )
%read_annotated_rhythm Returns an instance of RhythmDescription with the onset detection function, times of
%each beat, beats per measure, downbeat times, meter assigned from the ircambeat computed values.
% $Id$

if (nargin < 2)
    rhythm_directory_root = '~/Research/Data/IRCAM-Beat/Quaero_Selection/';
end

abs_rhythm_directory = tilde_expand(rhythm_directory_root);
odf_filepath = [abs_rhythm_directory 'Analysis/' filename '.odf'];
sample_rate = 172.27; % hardwired, again.
odf = ircam_odf(odf_filepath);

beat_markers_filepath = [abs_rhythm_directory 'Analysis/' filename '.wav.markers.xml'];
bpm_filepath = [abs_rhythm_directory 'Analysis/' filename '.wav.bpm.xml'];
[beat_times, beat_markers] = read_ircam_marker_times(beat_markers_filepath);
downbeats = find(beat_markers == 1); % downbeats are marked as "1".
anacrusis = downbeats(1) - 1; % number of beats before the first downbeat
start_from = beat_times(downbeats(1));
start_sample = round(sample_rate * start_from) + 1;

% TODO LMS commented out so we use the full ODF for matching
% odf_subset = odf(start_sample : end);
odf_subset = odf;

beats_per_measure = 4; % TODO hardwired! Need to read from .wav.markers.xml file
meter = [2 2 2 2]; % TODO hardwired!

% calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
tempo = 60/median(diff(beat_times)); 

fprintf('Starting downbeat finding from %.3f seconds, %.3f samples\n', start_from, start_sample);

analysed_rhythm = RhythmDescription(filename, odf_subset, beat_times, meter, beats_per_measure, anacrusis, sample_rate, tempo);
end
