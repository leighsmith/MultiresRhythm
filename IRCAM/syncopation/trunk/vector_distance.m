% -*- Octave *-*
function [ distances ] = vector_distance( vec1, vec2 )
%vector_distance Compute a distance matrix between two vectors, 
%   Returns a matrix with rows of vec1, columns vec2
%
% $Id$
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

distances = zeros(length(vec1), length(vec2));
  
for i = 1 : length(vec1)
    for j = 1 : length(vec2)
      distances(i, j) = abs(vec1(i) - vec2(j));
    end
end

end

