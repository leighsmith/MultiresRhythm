function [ cosine_distance ] = cosine_distance( a, b )
%cosine_distance Compute the normalised angular distance between two n-dimensional vectors.
%   Dot product divided by norms of the vectors. The value is inverted for
%   a distance between vectors, i.e. 0 is for identical vectors.

% cosine_distance = sum(a .* b) / (sqrt(sum(a .^ 2)) * sqrt(sum(b .^ 2)));
cosine_distance = 1 - sum(a .* b) / (norm(a) * norm(b));

end

