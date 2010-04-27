function [ absolute_filepath ] = ballroom( relative_filepath )
%ballroom Returns the path to the ballroom data set.

absolute_filepath = [tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/') relative_filepath];

end

