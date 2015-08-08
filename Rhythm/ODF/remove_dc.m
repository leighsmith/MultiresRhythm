function clean_signal = remove_dc(signal)
%remove_dc Remove any DC offset in the signal.
  clean_signal = signal .- mean(signal);
end
