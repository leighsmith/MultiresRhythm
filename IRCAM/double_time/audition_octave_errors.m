function [ output_args ] = audition_octave_errors( input_args )
%audition_octave_errors Summary of this function goes here
%   Detailed explanation goes here

% Read the double time examples.
octave_errors_fid = fopen(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/octave_errors.txt'), 'r');

while ~feof(octave_errors_fid)
    octave_error_filename = fgetl(octave_errors_fid);
    annotation_file = tilde_expand(['~/Local_Data/RWC_Auditions/', octave_error_filename '_beats_annotated.wav'])
    if ~exist(annotation_file, 'file')
        audition_rwc_file(octave_error_filename);
        audition_rwc_file(octave_error_filename, 'Analysis');
    end
end
fclose(octave_errors_fid);

end

