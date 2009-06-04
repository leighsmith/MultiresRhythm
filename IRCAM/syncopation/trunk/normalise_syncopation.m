function [ normalised_syncopation_measures ] = normalise_syncopation ( syncopation_measures, metrical_hierarchy )
%normalise_syncopation Returns the syncopation, normalising by the relative syncopation.

normalised_syncopation_measures = syncopation_measures ./ abs(min(lh_metric_salience(metrical_hierarchy)));

end

