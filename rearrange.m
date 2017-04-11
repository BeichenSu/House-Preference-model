final = zeros(p*q,3);
count = 0;
for i  = 1: 317
    for j = 1:743
        count = count + 1;
        final(count,1) = result(i,j);
        final(count,2) = X(i,j);
        final(count,3) = Y(i,j);
    end
end
xlswrite('FFinalData.xlsx',final);