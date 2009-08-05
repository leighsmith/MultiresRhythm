function [ closest_rhythms ] = closest_rhythms( similarity_matrix )
%closest_matches Returns the indices of the similarity matrix most closely
%matching each song per row.

% Add the maximum of each row to the diagonal so min() finds the values
% above the self-similarity value of 0.
no_self_sims = similarity_matrix + diag(ones(1, length(similarity_matrix)) .* max(similarity_matrix));
[lowest_distances, closest_rhythms] = min(no_self_sims);

end

