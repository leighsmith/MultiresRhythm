function [ next_pos ] = next_note_position (rhythm, position)
%next_note_position Summary of this function goes here
%   Detailed explanation goes here

next_pos = length(rhythm) - 1;
for pos = position + 1 : length(rhythm)
    if (is_note(rhythm(pos)))
		 next_pos = pos;
         break;
    end
end

end

