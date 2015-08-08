function [ syncopation_score ] = syncopation (note_weight, rest_weight) 
%syncopation Returns the amount of syncopation according to the metrical
%weights of the note and rest.
% $Id$

if(note_weight > rest_weight)
    syncopation_score = 0;
elseif (note_weight == rest_weight)
    syncopation_score = 1;
else
    syncopation_score = rest_weight - note_weight + 1;
end

end

