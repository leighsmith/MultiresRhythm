function [distance] = euclidean_distance(a, b)
%euclidean_distance Returns the Euclidean distance between two vectors.
     distance = sqrt(sum((a - b) .^ 2));
end
