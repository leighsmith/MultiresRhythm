function [ signal ] = sinbursts( frequency, bursts, duration, sample_rate )
%sinbursts Generate sinusoidal bursts for a specified duration.

signal_length = duration * sample_rate;
bursts_envelope = mod([0 : signal_length], signal_length / bursts) < (sample_rate / bursts);
signal = cos(2 * pi * frequency / sample_rate * [0 : signal_length]) .* bursts_envelope;

%plot(signal)
%wavwrite(signal, sample_rate, tilde_expand('~/Desktop/sinbursts.wav'));

end

