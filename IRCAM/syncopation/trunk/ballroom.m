function [ absolute_filepath ] = ballroom( relative_filepath )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

absolute_filepath = [tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/') relative_filepath];

end

