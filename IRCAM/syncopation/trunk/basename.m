function [ piece_name, filepath ] = basename( full_filepath )
%basename Returns the filename part of the file, 
%    Needed because fileparts differs from ircambeat's notion of extension, taking the last period as the
%    start of the extension, not the first.

    [filepath, filename] = fileparts(full_filepath);
    piece_name = strtok(filename, '.');

end

