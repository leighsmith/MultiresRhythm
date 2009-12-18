function [ quaero_rhythm ] = read_quaero_rhythm( filename, rhythm_directory_root, annotation_source )
%read_quaero_rhythm Just a wrapper around read_rhythm_description to handle
%the filepath creation.

abs_rhythm_directory = tilde_expand(rhythm_directory_root);
sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';
% sound_directory_root =  [abs_rhythm_directory 'Audio/']

annotated_beat_markers_filepath = [abs_rhythm_directory 'Annotation/' filename annotation_source '.xml'];
audio_filepath = [sound_directory_root filename '.wav']

quaero_rhythm = read_rhythm_description(annotated_beat_markers_filepath, audio_filepath);

end

