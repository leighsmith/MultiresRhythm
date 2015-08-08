function [ normalised_matrix ] = normalise( matrix )
%normalise Normalise over the entire matrix by scaling matrix values to lie between 0.0 and 1.0

minimum = min(min(matrix));
maximum = max(max(matrix));
range = maximum - minimum;
if range == 0
    range = maximum;
end

normalised_matrix = (matrix - minimum) / range;

end

