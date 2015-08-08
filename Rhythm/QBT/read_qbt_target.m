function [ rhythm_description ] = read_qbt_target( filename )
%read_qbt_target Reads from locations in the file system for the Query By Tapping data.

rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/QueryByTapping/');
sound_directory_root = [rhythm_directory_root 'Audio/'];

beat_markers_filepath = [rhythm_directory_root 'Analysis/' filename '.wav.markers.xml'];

audio_filepath = [sound_directory_root filename '.wav'];

rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath);

end


