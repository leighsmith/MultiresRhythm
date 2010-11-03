### -*- Octave -*-
###
### $Id$
###
### Plot the ridges in greyscale and the computed tactus in red, 
### expected tactus in blue.
###
function plotRidgesAndTactus(comment, ridges, computedTactus, tactusStartSample)
  magnitude_minimum = 0.001;
  ## The amount of downsampling of the cwt result on the translation axis
  ## we do before we plot.
  ## We could do away with this since we can process the data without
  ## excess resource strain, but the images usually exceed the width of
  ## a window, so this is mainly to provide a useful viewing aspect ratio.
  plotReduction = 4;

  ## Downsample the data 
  reduce = 1 : plotReduction : length(ridges);
  downSampledRidges = ridges(:, reduce);
  downSampledTactus = computedTactus(:, 1 : plotReduction : length(computedTactus));
  plotableTactus = zeros(size(downSampledRidges));

  greyscale = colormap("default");
  maxcolours = length(greyscale);
  maxRidgeColours = maxcolours - 1;

  ## red is for the ridge.
  red = [1.0 0.0 0.0];

  ## Create a color map that is a greyscale for all values except the
  ## topmost which is red.
  ridgeColourMap = [greyscale(1: length(greyscale) - 1, :); red];

  colormap(ridgeColourMap);

  maxridge = max(max(downSampledRidges));
  minridge = min(min(downSampledRidges));
  ridgerange = maxridge - minridge;
  plotableRidges = maxRidgeColours - (((downSampledRidges - minridge) ./ ridgerange) .* maxRidgeColours);

  rowsToIndent = floor((tactusStartSample) / plotReduction);
  ## Convert the column indexes into a fortran indexed vector
  rowMajorIndexedTactus = ((find(downSampledTactus) .+ rowsToIndent .- 1) * \
			  rows(downSampledRidges)) .+ downSampledTactus;

  plotableTactus(rowMajorIndexedTactus) = 1;

  plotableImage = plotableRidges .* ~plotableTactus + plotableTactus * maxcolours;
  namedImage("tactus", plotableImage, 1);
endfunction

