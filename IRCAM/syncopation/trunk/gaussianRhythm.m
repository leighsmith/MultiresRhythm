function [ gaussRhythm ] = gaussianRhythm( rhythm_signal, gaussWindowLength )
%gaussianRhythm Given a rhythm onset signal, returns a Gaussian window representation.

gaussianWindow = gauss(gaussWindowLength);
convolved = conv(rhythm_signal, gaussianWindow);
halfWindowLength = floor(length(gaussianWindow)/2);
% shift by half a window length so peak of the Gaussian is at impulse location.
gaussRhythm = convolved(halfWindowLength + mod(length(gaussianWindow), 2) : end - halfWindowLength);
% Plot the impulses and gaussians overlapped.
% plot(1:length(rhythm_signal), gaussTarget(1:length(rhythm_signal)), 1:length(rhythm_signal),  rhythm_signal)

end

