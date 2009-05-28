function [ normalised_syncopation_measures ] = syncopation_measures( onset_observations, hierarchy )
%syncopation_measures Calculate the syncopation values for each measure of
%onset observations.

dim = size(onset_observations);
rhythm_syncopation_measures = zeros(dim);

for measure_index = 1 : dim(2)
    % fprintf('measure index %d\n',measure_index); 
    measure_onsets = onset_observations(:,measure_index);
    % calculate_syncopations_on_grid(measure_onsets', hierarchy)'
    rhythm_syncopation_measures(:,measure_index) = calculate_syncopations_on_grid(measure_onsets', hierarchy)';
end

% rhythm_syncopation_measures
normalised_syncopation_measures = rhythm_syncopation_measures ./ abs(min(lh_metric_salience(hierarchy)));

end
