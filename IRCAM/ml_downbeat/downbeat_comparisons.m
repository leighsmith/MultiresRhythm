function [ output_args ] = downbeat_comparisons( filename )
%downbeat_comparisons Compares analysed and annotated versions of the same
%file.

dir_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

annotated_pattern = read_pattern([filename ' annotated'], [dir_root, 'Annotation/Pattern/', filename, '.pattern.xml']);
analysed_pattern = read_pattern([filename ' analysed'], [dir_root, 'Analysis/Pattern/', filename, '.pattern.xml']);

plot_pattern(annotated_pattern);
plot_pattern(analysed_pattern);

end
