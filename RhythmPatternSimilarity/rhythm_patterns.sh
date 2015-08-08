#!/bin/zsh
rm rhythm_patterns.txt
foreach f ($1/*.wav.bpm.xml)
  sed -n -e '/<rhythmpattern>/s/<\/*rhythmpattern>//gp' $f >> rhythm_patterns.txt
end
