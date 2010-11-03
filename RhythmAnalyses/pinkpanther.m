[mag, phase, pc] = multiresFile("Pink Panther without Accents\n", "~/AnalysedRhythms/pink-panther-unaccented");

fprintf(stderr, "at the 700th sample\n"); 
pause
plotScaleEnergy(mag, 700);
fprintf(stderr, "at the 1300th sample\n"); 
pause
plotScaleEnergy(mag,1300);
