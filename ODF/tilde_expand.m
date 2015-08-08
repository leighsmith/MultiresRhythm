function [ new_path ] = tilde_expand( path_with_tilde )
%tilde_expand Imitates the Octave function to make a Unix path independent of home directory.
%   Uses the POSIX environment variable $HOME.
% $Id: tilde_expand.m 5514 2009-06-11 10:40:17Z leighsmi $

new_path = regexprep(path_with_tilde, '~', getenv('HOME'));

end

