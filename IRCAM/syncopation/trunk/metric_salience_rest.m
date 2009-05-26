function [ max_rest ] = metric_salience_rest (position, rhythm, salience)
%metric_salience_rest Return highest metric_salience in note-to-note interval

search_region = (position + 1) : next_note_position(rhythm, position) - 1;
if(length(search_region) == 0)
    max_rest = 0;
else
    max_rest = max(salience(search_region));
end

end

