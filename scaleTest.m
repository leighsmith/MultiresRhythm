nScale = 112
voicesPerOctave = 16
newScale = 28

newIOI1 = timeSupport(nScale - newScale, voicesPerOctave)
newIOI2 = timeSupport(nScale - (newScale + 1), voicesPerOctave)
newIOI3 = timeSupport(nScale - (newScale - 1), voicesPerOctave)
newIOI1 - newIOI2
newIOI3 - newIOI1
newIOI3 - newIOI2

scaleDiff = 2 ^ (1/voicesPerOctave) - 1
newIOI1 * scaleDiff
scaleDiff = 1 - 2 ^ (-1/voicesPerOctave)
newIOI1 * scaleDiff
