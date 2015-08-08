function corpus_patterns = assign_styles( corpus_patterns, style_map )
%assign_styles Assign the style names from the style map.
%   Detailed explanation goes here

for i = 1 : length(corpus_patterns)
    corpus_patterns{i}.style = style_map(corpus_patterns{i}.name);
end

end

