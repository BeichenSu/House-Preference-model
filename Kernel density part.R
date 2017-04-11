setwd("C:/Users/subei_000/Desktop/404 Project")
newData  = read.table("Data.txt")
# Do a simple linear regression on ZRI with longi and lat
lfit = lm(newData[,1]~newData[,3]+newData[,4])
summary(lfit)
cor(cbind(newData[,1],newData[,3],newData[,4]))
# Clear to see the linear relationship between them is weak, and the latitude parameter is not sigificant
# This may suugest that the parametric regression is not appropriate.
# The kernel density estimation would work better

# Create grid points on longitude and latitude, now input 0.1 for test purpose, should be 0.001
# Due to that the data contains 15913 data points covering USA, the work load is really high and the
# function won't work
# So I put my focus on only part of California
# A box from longitude -121 to -116, latitude from 32 to 35

# Filter out the row to keep by new defined longitude
RTKlong = which(newData[,3] < -116 & newData[,3] > -121)
TTemp = newData[RTKlong,]
# Filter out the row by new defined latitude
RTKlati = which(TTemp[,4] > 32 & TTemp[,4] < 35)
SData = TTemp[RTKlati,]
# plot this part of data
plot(SData[,3],SData[,1], main = "ZRI vs Longitude", xlab = "longitude from -121 to -116",ylab = "ZRI")
plot(SData[,4],SData[,1], main = "ZRI vs Latitude", xlab = "latitude from 32 to 35", ylab = "ZRI")
# plot the location on map
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-124,-66), ylim = c(25,48), asp = 2, main = 'Location recorded on map')
points(newData[,3],newData[,4],col= 'red', cex = 0.2)

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-121,-116), ylim = c(32,35), asp = 2, main = 'Location recorded on map around CA')
points(SData[,3],SData[,4],col= 'red', cex = 0.6)



Glongi = seq(min(SData[,3]),max(SData[,3]),0.01)
Glati = seq(min(SData[,4]),max(SData[,4]),0.01)
# We have 443 points horizontal, and 244 vertical
p = length(Glongi)
q = length(Glati)

dyn.load("kdensity3d.dll")
b = bw.nrd(sqrt(SData[,3]^2 + SData[,4]^2))
kdens3d = function(x,y,z,gx,gy,q,b){
  n = length(x)
  p = length(gx)
  .C("kdensity3d",as.integer(n), as.integer(p), 
     as.double(x),as.double(y), as.double(z), as.double(gx), 
     as.double(gy), res = double(q), as.double(b))
}

A = kdens3d(SData[,3],SData[,4],SData[,1],Glongi,Glati[1],q,b)
#A = kdens3d(SData[,3],SData[,4],SData[,1],Glongi,Glati[1],q,b)
finalData = c()
for(i in 1: q/2){
  A = kdens3d(SData[,3],SData[,4],SData[,1],Glongi,Glati[i],q,b)
  finalData = cbind(finalData,A)
}
write.table(finalData, "C:/Users/subei_000/Desktop/404 Project/finalData.txt", row.names=FALSE)




