function [ output_args ] = audition_octave_errors( octave_errors_file )
%audition_octave_errors Summary of this function goes here
%   Detailed explanation goes here

% Read the double time examples.
octave_errors_filenames = read_filenames(octave_errors_file);

for i = 1 : length(octave_errors_filenames)
    octave_error_filename = octave_errors_filenames{i};
    annotation_file = tilde_expand(['~/Local_Data/RWC_Auditions/', octave_error_filename '_beats_annotated.wav'])
    if ~exist(annotation_file, 'file')
        audition_rwc_file(sprintf('~/Research/Data/IRCAM-Beat/RWC/Annotation/%s.beat.xml', octave_error_filename), 'annotated');
        audition_rwc_file(sprintf('~/Research/Data/IRCAM-Beat/RWC/%s/%s.wav.markers.xml', 'Analysis', octave_error_filename), 'tracked');
    end
end

end

