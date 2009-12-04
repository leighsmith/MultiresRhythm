function [ rhythm_description ] = read_qbt_query( filename )
%read_qbt_query Reads from locations in the file system for the Query By Tapping data.


rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/QueryByTapping/Query/');
sound_directory_root = rhythm_directory_root;

audio_filepath = [sound_directory_root filename '.wav'];

% beat_markers_filepath = [rhythm_directory_root 'Analysis/' filename '.wav.markers.xml'];

% rhythm_description = read_analysed_rhythm(beat_markers_filepath, audio_filepath);

% Kludged to only read the audio file ODF, not the beat markers.
[wideband_odf, sample_rate] = odf(audio_filepath);

beats_per_measure = 4; % TODO hardwired! Need to read from .wav.markers.xml file
meter = [2 2 2 2]; % TODO hardwired!

% calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
tempo = 120; % TODO hardwired 

% Supply wideband_odfs and no subbands to reduce memory.
rhythm_description = RhythmDescription(filename, wideband_odf, [], [], meter, beats_per_measure, 0, sample_rate, tempo);

end

