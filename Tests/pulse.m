### -*- Octave -*-
### Do the analysis on an impulse train.

signalLength = 2496;
## multiply the periodicity by two as the 1, -1 pair are samples of
## 0->Pi,Pi->2Pi phase regions respectively.
periodicity = 16;

## phase_mod = 32;
phase_mod = 1;
pulseTrain = zeros(1,signalLength);
pulseTrain(phase_mod : signalLength/periodicity : signalLength) = real(1 .^ [1:periodicity]);

pulseRhythm.signal = pulseTrain;
pulseRhythm.SampleRate = 200;
pulseRhythm.description = "Isochronous Impulse Train";

analysis.beginClapAt = 1;
analysis.expectedTactusIOI = 0;
analysis.multiple = 1;
analysis.correlationMethod = "rd | rlp | rsp";

[mag, phase, pc, tactus, claps] = multiresRhythm(pulseRhythm, analysis, "");

## multiresFile("Isochronous Impulse File", "~/AnalysedRhythms/genpulse_octave", 128, 1, "rd | rlp | rsp");

# plot(pc)
# # plot(real(c),"1",imag(c),"2")
# pause
# plot(flipud(mag(:,714)))

## [mag, phase, pc] = multiresFile("Series of pulses\n", "~/AnalysedRhythms/pulse");
