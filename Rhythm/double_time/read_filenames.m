function [ filenames ] = read_filenames( file )
%read_filenames Returns the filenames in the given text file.

filenames = cell(1, 5);

% Read the double time examples.
fid = fopen(tilde_expand(file), 'r');
line_index = 1;
while ~feof(fid)
    filenames{line_index} = fgetl(fid);
    line_index = line_index + 1;
end
fclose(fid);

end

