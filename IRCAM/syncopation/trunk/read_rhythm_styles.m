function [ styles ] = read_rhythm_styles( filename )
%read_rhythm_styles Summary of this function goes here
%   Detailed explanation goes here

style_file = fopen(tilde_expand(filename));
styles = containers.Map();

while ~feof(style_file)
    style_filename = fscanf(style_file, '"%[A-Za-z-_0-9]",');
    styles(style_filename) = fscanf(style_file, '%[A-Za-z]\n');
end

end

