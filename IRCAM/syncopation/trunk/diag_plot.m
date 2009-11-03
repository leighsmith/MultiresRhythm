% Abstracts conditional diagnostic plotting.
% $Id$
    
% plotting = {'onset_observations'};
% plotting = {'syncopation_profile' 'syncopation_measures' 'syncopation_variation'};
% plotting = {'syncopation_profile' 'syncopation_measures'};
% plotting = {'gap_evaluation'}; 
% plotting = {'silence_observations'};
% plotting

function [ should_plot ] = diag_plot(test_case)
    %diag_plot Checks if the plotting array holds the test_case, returning true or false.
    global plotting;
    
    should_plot = ~isempty(strmatch(test_case, plotting));
end
