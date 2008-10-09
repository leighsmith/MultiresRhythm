## -*- Octave -*-
##
## Ritarding and accelerating examples

[mag_rit, phase, pc] = multiresFile("Ritard without accent\n", "~/AnalysedRhythms/ritard");

## Now 4/4 intensified accent version
[mag_rit44, phase, pc] = multiresFile("Ritard 4/4 accent version\n", "~/AnalysedRhythms/ritard44");

fprintf(stderr, "Hit a key to plot mag cross-sections\n");
pause
plot(flipud(mag_rit(:,4200)), "3", flipud(mag_rit44(:,4200)), "4")
pause
plot(flipud(mag_rit(:,2428)), "2", flipud(mag_rit44(:,2428)), "1")

