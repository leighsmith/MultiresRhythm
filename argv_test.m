#! /usr/local/bin/octave -qf
printf ("%s", program_name);
for i = 1:nargin
  printf (" %s", argv(i,:));
endfor
printf ("\n");
