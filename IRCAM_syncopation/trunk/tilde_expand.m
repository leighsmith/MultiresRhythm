function [ new_path ] = tilde_expand( path_with_tilde )
%tilde_expand Imitates the Octave function to make a Unix path independent of home directory.
%   Uses the POSIX environment variable $HOME.
% $Id$

new_path = regexprep(path_with_tilde, '~', getenv('HOME'));

end

