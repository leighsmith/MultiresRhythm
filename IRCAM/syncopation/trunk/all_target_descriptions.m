function [ all_target_descriptions ] = all_target_descriptions( corpus_directory )
%all_target_descriptions Return a cell array of RhythmDescriptions read
%from the QBT targets.

target_directory = tilde_expand([corpus_directory '/Audio']);
target_files = dir(target_directory);

target_descriptions = cell(length(target_files), 1);

target_index = 1;
for file_index = 1 : length(target_files)
    target_pathname = target_files(file_index).name;
    if (target_pathname(1) ~= '.') % Remove all hidden files.
        [target_file, extension] = strtok(target_pathname, '.');

        if(strcmp(extension, '.wav'))
            fprintf('%s%s\n', target_file, extension);
            target_descriptions{target_index} = read_qbt_target(target_file);
            target_index = target_index + 1;
        end
    end
end

all_target_descriptions = target_descriptions(1 : target_index - 1);

end

