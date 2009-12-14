function [ all_query_descriptions ] = all_query_descriptions( corpus_directory )
%all_query_descriptions Returns all the query descriptions of the wav files
%in the directory.
% $Id$

query_directory = tilde_expand([corpus_directory '/Query']);
query_files = dir(query_directory);

query_descriptions = cell(length(query_files), 1);

query_index = 1;
for file_index = 1 : length(query_files)
    query_pathname = query_files(file_index).name;
    if (query_pathname(1) ~= '.') % Remove all hidden files.
        [query_file, extension] = strtok(query_pathname, '.');

        if(strcmp(extension, '.wav'))
            fprintf('%s%s\n', query_file, extension);
            query_descriptions{query_index} = read_qbt_query(query_file);
            query_index = query_index + 1;
        end
    end
end

all_query_descriptions = query_descriptions(1 : query_index - 1);

end
