function [ output_args ] = write_filenames( filename, all_files )
%write_filenames Create a text file with the file names

fod = fopen(filename, 'w');
for i = 1 : length(all_files)
    fprintf(fod, '%s\n', basename(all_files{i}));
end
fclose(fod);

end

