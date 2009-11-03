function [ annotated_rhythm ] = read_quaero_rhythm( filename, rhythm_directory_root, annotation_source )
%read_quaero_rhythm Just a wrapper around read_annotated_rhythm to handle
%the filepath creation.

sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';

abs_rhythm_directory = tilde_expand(rhythm_directory_root);
annotated_beat_markers_filepath = [abs_rhythm_directory 'Annotation/' filename annotation_source '.xml'];
audio_filepath = [sound_directory_root filename '.wav']
% audio_filepath = [abs_rhythm_directory 'Audio/'  filename '.wav'];

annotated_rhythm = read_annotated_rhythm(annotated_beat_markers_filepath, audio_filepath);

end

