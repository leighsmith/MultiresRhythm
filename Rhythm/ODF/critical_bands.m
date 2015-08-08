function critical_bands(samplerate)
    numOfBands = 6;
    % TODO Need critical bands on the Bark scale of Zwicker & Fastl. numOfBands = 20
    % Calculate a log scaling of the available spectrum in the range of 100Hz - sr
    criticalBandRegion = log(samplerate/2 - 100) / numOfBands;
    spans = 100 + criticalBandRegion ^ (1 : numOfBands)
end
