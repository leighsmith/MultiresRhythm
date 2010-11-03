## -*- Octave -*-
##
## $Id$
function plotRidgeAndCWT(comment, ridge, magnitude, phase)
  magnitude_minimum = 0.001;

  grayscale = colormap("default");
  maxcolors = length(grayscale);

  ## use a colormap which has the lowest value white, for
  ## ill-conditioned phase values, the rest a spectral distribution
  ## red -> violet.
  white = [1.0 1.0 1.0];

  ## black is for the ridge.
  black = [0.0 0.0 0.0];

  colormap([white; spectral_colormap(maxcolors - 2); black]);
  
  ## phase assumed [-pi -> pi], map it to [2 -> maxcolors-1],
  ## when magnitude < magnitude_minimum, set the phase to 1, 
  ## the first colormap index.
  plotable_phase = ((magnitude > magnitude_minimum) .* \
		    ceil(((phase / pi + 1) / 2) .* (maxcolors - 2))) + 1;

  ## Convert the column indexes into a fortran indexed vector
  fortranIndexedRidge = ((find(ridge) - 1) * \
			  rows(plotable_phase)) + ridge;
  plotableRidge(fortranIndexedRidge) = 1;

  plotable_image = plotable_phase .* ~plotableRidge + plotableRidge * maxcolors;
  namedImage("phase_and_ridge", plotable_image, 1);
endfunction
