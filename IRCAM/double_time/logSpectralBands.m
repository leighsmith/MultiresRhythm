function [ spectralBands ] = logSpectralBands( numberOfBands, lowestFrequency, highestFrequency )
%logSpectralBands Returns the given number of bands, spread logarithmically
%across the given frequency range.

logFreqRange = log(highestFrequency) - log(lowestFrequency);
spectralBands = exp(log(lowestFrequency) : logFreqRange / numberOfBands : log(highestFrequency));

end

