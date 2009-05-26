function [ salience ] = lh_metric_salience (meter)
%lh_metric_salience calculating metric salience the Longuet-Higgins & Lee way
%   Return list of metric weights; 0 highest level, -n lowest.

grid_length = prod(meter);
salience = zeros(1, grid_length);
level = -1;
interval = grid_length;
for division_index = 1 : length(meter)
    division = meter(division_index);
    interval = interval / division;
    for pos = 1 : interval : grid_length
        if(salience(pos) == 0)
            salience(pos) = level;
        end
    end
    level = level - 1;
end

salience(1) = 0; % top most level

end
