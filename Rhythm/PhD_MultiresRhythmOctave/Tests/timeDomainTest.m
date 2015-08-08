t = [-5:0.01:5];
g = exp(- t.^2/2);
e = exp(i .* t .* 5);
w = g .* e;
plot(real(w), "1", imag(w), "2")
wf = fft(w);
plot(wf);
	
