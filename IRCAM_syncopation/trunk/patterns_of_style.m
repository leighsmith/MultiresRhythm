function [ selected_patterns ] = patterns_of_style ( corpus_patterns, style )
%patterns_of_style Summary of this function goes here
%   Detailed explanation goes here

selected_patterns = cell(10, 1);
write_index = 1;

for i = 1 : length(corpus_patterns)
    if(strcmp(corpus_patterns{i}.style, style))
        selected_patterns{write_index} = corpus_patterns{i};
        write_index = write_index + 1;
    end
end

end

