% Abstracts conditional diagnostic plotting.
% $Id$
    
% plotting = {'onset_observations'};
% plotting = {'syncopation_profile' 'syncopation_measures' 'syncopation_variation'};
% plotting = {'syncopation_profile' 'syncopation_measures'};
% plotting = {'gap_evaluation'}; 
% plotting = {'silence_observations'};
% plotting

function [ should_plot ] = diag_plot(test_case)
    %diag_plot Checks if the plotting array holds the test_case, returning 1 or 0.
    global plotting;
    
    should_plot = strmatch(test_case, plotting);

end
