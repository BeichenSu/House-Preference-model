clear all;close all; clc
load smallData.txt;
z = smallData(:,1);
x = smallData(:,3);
y = smallData(:,4);
%scatter(x,y)
gx = (min(x):0.01:max(x));
gy = (min(y):0.01:max(y));
b = 0.331627;
p = length(gx);
q = length(gy);
n = length(x);
result = zeros(742,317);
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
[X,Y] = meshgrid(gx,gy);
result = transpose(result);
h = surface(X,Y,result);
set(h,'LineStyle','none')
xlswrite('FinalData.xlsx',result);
[C,h] = contour(X,Y,result);   
clabel(C,h)
