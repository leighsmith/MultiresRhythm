function [ odfBands ] = odfSpectralBands( numberOfBands )
%odfSpectralBands Return the spectral bands used to compute spectral
%subband metrical profiles.

cutOffPoints = round(logSpectralBands(numberOfBands, 60, 11025 / 2));
odfBands = [cutOffPoints(1 : end - 1); cutOffPoints(2 : end)]';

end

