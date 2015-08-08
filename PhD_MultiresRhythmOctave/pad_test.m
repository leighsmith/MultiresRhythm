[rhythm_signal, header] = loadRhythm("~/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium", "snd");
[pad_signal, trim] = dyadicPad(rhythm_signal);

pad_signal_size = size(pad_signal)
size_of_trimmed_padded = size(pad_signal(:, trim))
size_of_signal = size(rhythm_signal)

trimmed_padded_signal = pad_signal(:, trim);

trim(1)
trim(length(trim))

find(rhythm_signal)
find(pad_signal)
find(trimmed_padded_signal)


## rhythm_signal == pad_signal(:, trim)


