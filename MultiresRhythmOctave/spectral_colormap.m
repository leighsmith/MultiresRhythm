### -*- Octave -*-
### $Id$
###
### create a colormap of length n which is a spectral distribution,
### the colors forming a hue wheel.
### Leigh Smith - leigh@cs.uwa.edu.au
###
function hls = spectral_colormap(n)
  if(nargin < 1)
     n = 64;
  endif
  light(1 : n) = 0.5;
  saturation(1 : n) = 1.0;
  hls = hls_to_rgb([1 : 360 / n : 360], light, saturation);
endfunction
