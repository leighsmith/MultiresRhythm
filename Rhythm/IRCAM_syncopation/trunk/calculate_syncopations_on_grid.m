function [ syncopation_score ] = calculate_syncopations_on_grid (rhythm, meter)
%calculate_syncopations_on_grid Return list of syncopation strengths from a binary valued rhythm vector.
% $Id$

rhythm = [rhythm, 1]; % place extra onset for following downbeat.
 
salience = lh_metric_salience(meter);
syncopation_score = zeros(1,length(salience));
for position = 1 : length(salience)
    % if a note, followed by rest, retrieve the associated syncopation salience.
    if (is_note(rhythm(position)) && not(is_note(rhythm(position + 1))))
        % metric_salience note
        syncopation_score(position) = syncopation(salience(position), metric_salience_rest(position, rhythm, salience));
    else
        syncopation_score(position) = 0;
    end
end

