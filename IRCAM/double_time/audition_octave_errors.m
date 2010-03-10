function [ output_args ] = audition_octave_errors( octave_errors_file )
%audition_octave_errors Summary of this function goes here
%   Detailed explanation goes here

% Read the double time examples.
octave_errors_filenames = octave_error_files(octave_errors_file);

for i = 1 : length(octave_errors_filenames)
    octave_error_filename = octave_errors_filenames{i};
    annotation_file = tilde_expand(['~/Local_Data/RWC_Auditions/', octave_error_filename '_beats_annotated.wav'])
    if ~exist(annotation_file, 'file')
        audition_rwc_file(octave_error_filename);
        audition_rwc_file(octave_error_filename, 'Analysis');
    end
end

end

