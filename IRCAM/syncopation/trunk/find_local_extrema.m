function [ indices_of_extrema ] = find_local_extrema( vector, extrema )
%find_local_maxima Returns the indices of the vector which are maximal
%   Detailed explanation goes here

extrema_values = diff(sign(diff(vector)));

if (strcmp(extrema, 'max'))
    comparison = extrema_values < 0;
else
    comparison = extrema_values > 0;
end
    
indices_of_extrema = find(comparison);

if (length(indices_of_extrema) > 0)
    % Add 1 for the position reduction due to diff, 1 for base 1 array indexing.
   indices_of_extrema = indices_of_extrema + 1;
end

end

