function [ rotated_query ] = hypermeter_align( query_hypermeter, target_hypermeter )
%hypermeter_match Returns a match between the hypermeters.
%   Performs a rotation to find the best alignment of the hypermetrical profiles.
% $Id$

best_rotations = cross_correlation_match(query_hypermeter, target_hypermeter);
% plot_correlation_match(query_hypermeter, target_hypermeter, best_rotations(1));
% pause
if(best_rotations(1) == 0)
    rotated_query = query_hypermeter;
else
    fprintf('Rotated hypermetrical profile by %d\n', best_rotations(1));
    rotated_query = [query_hypermeter(best_rotations(1) : end) query_hypermeter(1 : best_rotations(1) - 1)];
end

end

