function [ metric_grid ] = metric_grid_from_probabilities ( tatum_probabilities )
%metric_grid_from_probabilities Returns a binary grid from probabilities of
%relative silence in each tatum position.
% $Id$

dim = size(tatum_probabilities);
grid_length = dim(1); % number of rows = number of tatums in measure.
threshold = 1.0 / grid_length;
metric_grid = tatum_probabilities < threshold;

end
