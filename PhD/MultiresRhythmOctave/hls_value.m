# -*- Octave -*-
#
# Sub-function to convert from hue (0-360 degrees) and positions within the 
# colour pyramid to a value usable for conversion to RGB.
# From Van Dam and Foley (1982) p619.
# Leigh Smith - leigh@cs.uwa.edu.au
#
function value = hls_value(n1, n2, hue)
  for i = 1:length(hue)
    if hue(i) > 360
      hue(i) = hue(i) - 360;
    endif
    if hue(i) < 0
      hue(i) = hue(i) + 360;
    endif
    if hue(i) < 60
      value(i) = n1(i) + (n2(i) - n1(i)) .* hue(i) / 60;
    elseif hue(i) < 180
      value(i) = n2(i);
    elseif hue(i) < 240
      value(i) = n1(i) + (n2(i) - n1(i)) .* (240 - hue(i)) / 60;
    else
      value(i) = n1(i);
    endif
  endfor
endfunction
