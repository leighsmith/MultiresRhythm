## -*- Octave -*-
## Parncutt (1994) equation 3 for durational accent. 

## in milliseconds.
saturationDuration = 500
ioi = 0 : 100 : 2000;
## determines the minimum discriminable IOI
accentIndex = 1:1:4

durationalAccent(:,accentIndex) = (1 - exp(-ioi / saturationDuration)) .^ accentIndex 

gset term aqua
## plot all three with different values of accentIndex
## plot(durationalAccent[1], "", durationalAccent[2], "", durationalAccent[3], "")
plot(durationalAccent, "")

## Equation 6 for pulse-period salience function defining an existence
## region of pulse sensation.

## in milliseconds.
moderatePulsePeriod = 600
##
pulsePeriodStdDev = 0.2


pulsePeriodSalience = exp(-0.5 * ((log10(p / moderatePulsePeriod) / sigma) ^ 2))
