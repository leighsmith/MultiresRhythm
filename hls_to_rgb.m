## -*- Octave -*-
##
## $Id$
##
## Function to convert from hue (0-360 degrees) lightness (0-1) and
## saturation (0-1) to RGB values. From Van Dam and Foley (1982) p617.
## Leigh Smith - leigh@cs.uwa.edu.au
##
function rgb = hls_to_rgb(hue, lightness, saturation)
  # assume lightness > 0.5
  m2 = lightness + saturation - lightness .* saturation;
  m1 = 2 .* lightness - m2;
  if(saturation != 0)
    r = hls_value(m1, m2, hue + 120);
    g = hls_value(m1, m2, hue);
    b = hls_value(m1, m2, hue - 120);
  endif
  rgb = [r.', g.', b.'];
endfunction
