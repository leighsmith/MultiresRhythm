function [ output_args ] = select_quaero()
%select_quaero Summary of this function goes here
%   Detailed explanation goes here

select_quaero_names = make_quaero_dataset(100, 1, 'Quaero_Selection');

% Write out new syncopation measures.
select_syncopation = syncopation_for_corpus(select_quaero_names);

% TODO Hardwired at 4 tatums per beat.
similarity_matrix = syncopation_similarity(strip_beats(select_syncopation, 4));

for i = 1 : length(select_quaero_names)
    fprintf('%d: %s\n', i, select_quaero_names{i})
end

figure();
imagesc(similarity_matrix)
title('Dissimilarity of Syncopation of Quaero Selection') 

figure();
bar(sum(similarity_matrix));
title(sprintf('Dissimilarity of Quaero Selection'));

end

