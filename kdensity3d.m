function kdensity3d = myfunction(x,y,z,gx,gy,b)
     n = length(x);
     p = length(gx);
     q = length(gy);
     result = zeros(244,443);
     for i = 1:p
         for j = 1:q
             a1 = 0;
             a2 = 0;
             for k = 1:n
                 u = sqrt((x(k) - gx(i))^2 + (y(k)-gy(j))^2)/b;
                 c = 1/(sqrt(2*pi))*exp(-0.5*(u^2))/b;
                 a1 = a1 + z(k)*c;
                 a2 = a2 + c;
             end
             result(i,j) = a1/a2;
         end
     end
     kdensity3d = result;

