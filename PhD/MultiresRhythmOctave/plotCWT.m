## -*- Octave -*-
## $Id$
##
## Function to plot the magnitude and phase components of the result of
## a continuous wavelet transform on a signal.
## Assumes the magnitude value is positive, phase is -pi -> pi.
## Ill-conditioned phase measures due to low magnitude are displayed as white.
## maglimit can be used to clamp the global extrema at limits to allow
## interpreting the magnitude density plots for local extrema.
## TODO:would be nice to use saturation to indicate magnitude value
## on the phase plot.
##
function plotCWT(name, magnitude, phase, maglimit)
  magnitude_minimum = 0.001;
  ## The amount of downsampling of the cwt result on the translation axis
  ## we do before we plot.
  ## We could do away with this when we can process the data
  ## without excess resource strain, but it makes for  diagrams
  ## which are not so wide, making them easier to view and interpret.
  plotReduction = 4;

  grayscale = colormap("default");
  maxcolors = length(grayscale);

  ## Downsample the data 
  reduce = 1 : plotReduction : length(magnitude);
  downSampledMagnitude = magnitude(:, reduce);

  ## A problem can be that the dynamic range of the signal energy can
  ## exceed the grey scales, making most of the interesting local maxima
  ## barely observable due to the "height" of the global maxima.
  ## Therefore we allow clamping the magnitude at a given limit.
  maxmag = max(max(downSampledMagnitude));	# what it would have been
  if(nargin > 3)
    exceeded = downSampledMagnitude > maglimit;
    downSampledMagnitude = !exceeded .* downSampledMagnitude + exceeded .* maglimit;
  endif
  maxmag = max(max(downSampledMagnitude))
  minmag = min(min(downSampledMagnitude))
  magrange = maxmag - minmag;
  plotable_mag = maxcolors - (((downSampledMagnitude - minmag) ./ magrange) .* maxcolors);
  namedImage([name, "_magnitude"], plotable_mag, 1)

  if(nargin > 2)
    ## use a colormap which has the lowest value white, for
    ## ill-conditioned phase values, the rest a spectral distribution
    ## red -> violet.
    white = [1.0 1.0 1.0];
    colormap([white; spectral_colormap(maxcolors - 1)]);

    downSampledPhase = phase(:, reduce);
 
    ## phase assumed [-pi -> pi], map it to [2 -> maxcolors],
    ## when downSampledMagnitude < magnitude_minimum, set the phase to 1, 
    ## the first colormap index.
    plotable_phase = ((downSampledMagnitude > magnitude_minimum) .* \
             ceil(((downSampledPhase / pi + 1) / 2) .* (maxcolors - 1))) + 1;
    namedImage([name, "_phase"], plotable_phase, 1)
  endif
endfunction
