function fn = discrete_signal(start, endvalue, sr)
  fn = start : (endvalue - start) / sr : endvalue;
endfunction

