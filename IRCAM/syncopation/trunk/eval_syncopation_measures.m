function [ syncopation_profile ] = eval_syncopation_measures ( analysed_rhythm )
%eval_syncopation_measures Returns a metrical profile of syncopation
%measures for the given rhythm.
plotting = {};

rhythm_name = analysed_rhythm.name;
onset_observations = observe_onsets(analysed_rhythm);
dim = size(onset_observations);
num_of_measures = dim(2);
metric_profile = sum(onset_observations') ./ num_of_measures;
rhythm_syncopation_measures = syncopation_measures(onset_observations, analysed_rhythm.meter);
% TODO, we can normalise the syncopation measure, to produce an equally
% comparable measure for each tatum, however this seems counter to the goal
% of the measure, which is to measure the effect at different locations
% across the measure.
normalised_syncopation_measures = normalise_syncopation(rhythm_syncopation_measures, analysed_rhythm.meter);
dim = size(normalised_syncopation_measures);
num_of_tatums = dim(1); % recalc this in case the syncopation measures differ from the tatums.
syncopation_profile = sum(normalised_syncopation_measures') ./ num_of_measures;

%% fprintf('metric profile %s syncopation_profile %s~%', metric_profile, syncopation_profile)

if (strmatch('syncopation_profile', plotting))
    figure();
    bar([metric_profile; syncopation_profile]');
    title(sprintf('Metric profile of %s', rhythm_name));
    % 		    :legends '('Beat Occurrence' 'Syncopation Intensity')
    %       	:xlabel 'Metric Location'
    % 		    :ylabel 'Relative Occurrence in Piece')
    % close();
end

if (strmatch('syncopation_measures', plotting))
    figure();
    imagesc(rhythm_syncopation_measures)
    % 	   :aspect_ratio 0.66
    title(sprintf('Syncopation intensity for %s', rhythm_name));
    %close();
end

if (strmatch('syncopation_variation', plotting))
    syncopation_variation = sum(normalised_syncopation_measures) ./ num_of_tatums;

    figure();
    plot(syncopation_variation);
    % 	  :aspect_ratio 0.66
    title(sprintf('Evolution of syncopation of %s', rhythm_name))
    % close();
end

end

