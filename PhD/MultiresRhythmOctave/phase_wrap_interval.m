## -*- Octave -*-
## the phase time support
## determine the length of the time support of the phase by
## determining distance between zero crossings.
function intervals = phase_wrap_interval(dt_phase, scale)
  wrap = abs(dt_phase) > 2;
  ## find on the transpose of the matrix gives the result sorted by
  ## scale rather than by time.
  ## [wrap_times, wrap_scales] = find(wrap.');
  wrap_times = find(wrap(scale,:));
  intervals = diff(wrap_times);
endfunction

