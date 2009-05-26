function [ syncopation_profile ] = eval_syncopation_measures ( analysed_rhythm )
%eval_syncopation_measures Returns a metrical profile of syncopation
%measures for the given rhythm.

rhythm_name = analysed_rhythm.name;
onset_observations = observe_onsets(analysed_rhythm);
dim = size(onset_observations);
num_of_measures = dim(2);
metric_profile = sum(onset_observations') ./ num_of_measures;
rhythm_syncopation_measures = syncopation_measures(onset_observations, analysed_rhythm.meter);
dim = size(rhythm_syncopation_measures);
num_of_tatums = dim(1); % recalc this in case the syncopation measures differ from the tatums.
syncopation_profile = sum(rhythm_syncopation_measures') ./ num_of_measures;
syncopation_variation = sum(rhythm_syncopation_measures) ./ num_of_tatums;
     
%% fprintf('metric profile %s syncopation_profile %s~%', metric_profile, syncopation_profile)

figure()
% (plot_histogram (make_narray (list metric_profile syncopation_profile)) nil 
bar([metric_profile; syncopation_profile]');
title(sprintf('Metric profile of %s', rhythm_name));
% 		    :legends '('Beat Occurrence' 'Syncopation Intensity')
% 		    :xlabel 'Metric Location'
% 		    :ylabel 'Relative Occurrence in Piece')
% close();

figure();
imagesc(rhythm_syncopation_measures)
% 	   :aspect_ratio 0.66
title(sprintf('Syncopation intensity for %s', rhythm_name));
%close();

figure();
plot(syncopation_variation);
% 	  :aspect_ratio 0.66
title(sprintf('Evolution of syncopation of %s', rhythm_name))
% close();

end

