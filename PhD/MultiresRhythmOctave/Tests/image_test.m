s = cos(discrete_signal(0, 2 .* pi, 511)) / 500;
max(s)
size(s)
p = zeros(256, 512);
for i = 1:256
 p(i, :) = s;
endfor

grayscale = colormap("default");
maxcolors = length(grayscale);
plotable_mag = maxcolors - (p .* (maxcolors ./ max(max(p))));
image(plotable_mag, 1)
