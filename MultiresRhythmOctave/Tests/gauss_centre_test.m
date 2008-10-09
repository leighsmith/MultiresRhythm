nScale = 120
b = zeros(nScale, 5);
b(100,1) = 1;
b(50,2) = 1;
b(40,3) = 1;
b(2,4) = 1;
b(70,5) = 1;

conv = gaussCentered(b, 16);

plot(real(conv(:,1)));
pause
plot(real(conv(:,2)));
pause
plot(real(conv(:,3)));
pause
plot(real(conv(:,4)));
pause
plot(real(conv(:,5)));
