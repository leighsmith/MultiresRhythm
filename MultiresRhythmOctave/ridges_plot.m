## -*- Octave -*-
## Function to plot the magnitude and phase components of the result of
## a continuous wavelet transform on a signal.
## Assumes the magnitude value is positive, phase is -pi -> pi.
## Ill-conditioned phase measures due to low magnitude are displayed as white.
## maglimit can be used to clamp the global extrema at limits to allow
## interpreting the magnitude density plots for local extrema.
## TODO:would be nice to use saturation to indicate magnitude value
## on the phase plot.
##
function ridges_plot(name, magnitude)
  magnitude_minimum = 0.001;

  grayscale = colormap("default");
  maxcolors = length(grayscale);

  ## A problem can be that the dynamic range of the signal energy can
  ## exceed the grey scales, making most of the interesting local maxima
  ## barely observable due to the "height" of the global maxima.
  ## Therefore we allow clamping the magnitude at a given limit.
  maxmag = max(max(magnitude));	# what it would have been
  if(nargin > 2)
    exceeded = magnitude > maglimit;
    magnitude = !exceeded .* magnitude + exceeded .* maglimit;
  endif
  maxmag = max(max(magnitude))
  minmag = min(min(magnitude))
  magrange = maxmag - minmag;
  plotable_mag = maxcolors - (((magnitude - minmag) ./ magrange) .* maxcolors);
  namedImage(["ridges", name], plotable_mag, 1)

endfunction


