function [ gaussRhythm ] = gaussianRhythm( rhythm_signal, gaussWindowLength )
%gaussianRhythm Given a rhythm onset signal, returns a Gaussian window representation.

gaussianWindow = gauss(gaussWindowLength);
% shift so peak of Gaussian is at impulse location.
convolved = conv(rhythm_signal, gaussianWindow);
halfWindowLength = floor(length(gaussianWindow)/2);
gaussRhythm = convolved(halfWindowLength + mod(length(gaussianWindow), 2) : end - halfWindowLength);
% Plot the impulses and gaussians overlapped.
% plot(1:length(rhythm_signal), gaussTarget(1:length(rhythm_signal)), 1:length(rhythm_signal),  rhythm_signal)

end

