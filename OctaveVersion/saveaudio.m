## Copyright (C) 1996 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage:  saveaudio (name, X, [, ext [, bit]])
##
## Saves a vector X of audio data in the file "name.ext".
## The format of the audio file is determined by ext which has to be
## written without an inital ".";  default value for ext is "lin".
##
## Currently, the following audio formats are supported:
## *) mu-law files with extension "mu", "au" or "snd"
## *) linearly encoded files with extension "lin" or "raw"
## If the data is saved linearly, the bit argument decides whether an
## 8-bit (default) or a 16-bit format is used.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 5 September 1994
## Adapted-By: jwe

function saveaudio (name, X, ext, bit)

  if (nargin < 2 || nargin > 4)
    usage ("saveaudio (X, name [, ext [, bit]])");
  endif

  if (nargin == 2)
    ext = "lin";
  endif

  if (nargin < 4)
    bit = 8;
  elseif (bit != 8 && bit != 16)
    error ("saveaudio: bit must be either 8 or 16");
  endif

  [nr, nc] = size (X);
  if (nc != 1)
    if (nr == 1)
      X = X';
      nr = nc;
    else
      error ("saveaudio: X must be a vector.");
    endif
  endif

  num = fopen ([name, ".", ext], "w");

  if (strcmp (ext, "lin") || strcmp (ext, "raw"))
    if (bit == 8)
      ld = max (abs (X));
      if (ld > 127)   # convert 16 to 8 bit
	if (ld < 16384)
	  sc = 64 / ld;
	else
	  sc = 1 / 256;
	endif
	X = fix (X * sc);
      endif
      X = X + 127;
      c = fwrite (num, X, "uchar");
    else
      c = fwrite (num, X, "short");
    endif
  elseif (strcmp (ext, "mu") || strcmp (ext, "au") || strcmp (ext, "snd"))
    Y = lin2mu (X);
    c = fwrite (num, Y, "uchar");
  else
    fclose (num);
    error ("saveaudio does not support given extension");
  endif

  fclose (num);

endfunction
