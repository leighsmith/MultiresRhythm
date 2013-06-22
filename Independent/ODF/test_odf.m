sines = sinbursts(100, 3, 10, 11025);
[wodf, odf_sr, subband_odfs] = odf_of_signal(sines', 11025, [[60, 100]; [3500, 4000]]);
