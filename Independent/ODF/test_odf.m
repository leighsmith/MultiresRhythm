sines = sinbursts(100, 3, 10, 11025);
[wodf, odf_sr, subband_odfs] = odf_of_signal(sines', 11025, [[60, 100]; [3500, 4000]]);

filename = "simpleLoop.wav";
[audio_signal, sample_rate, resolution] = wavread(tilde_expand(filename));
num_audio_channels = size(audio_signal, 2); % determine from signal.
mono_audio_signal = sum(audio_signal, 2) / num_audio_channels;
times = onset_times(filename)
figure()
% plot(mono_audio_signal, "1")
plot(mono_audio_signal, "1", times .* sample_rate, ones(1,length(times)), "2^")
