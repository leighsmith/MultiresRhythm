function [ syncopation_profile ] = plot_syncopation( name_of_piece )
%plot_syncopation Calculate the syncopation and plot it.
% $Id$

    analysed_rhythm = read_rhythm_description(name_of_piece);
    syncopation_profile = eval_syncopation_measures(analysed_rhythm);

end

