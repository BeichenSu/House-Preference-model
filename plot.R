setwd("C:/Users/subei_000/Desktop/404 Project")
ZLL = read.csv("zipcode.csv")
# Load the zillow data
mydata = read.csv("zillow.csv")
# Load zipcode of the real data points
zipcode = mydata$RegionName
# Load the ZRI of the real data points
ZRI = mydata$Zri
# Find the longitude and latitude of those points
# If cannot find the longitude and latitude, set -1 indicating no found
longitude = c()
latitude = c()
for (i in 1:15916) {
  if (length(ZLL[which(ZLL$Zipcode == zipcode[i]),]$Long) > 0) {
    longitude[i] = ZLL[which(ZLL$Zipcode == zipcode[i]),]$Long
    latitude[i] = ZLL[which(ZLL$Zipcode == zipcode[i]),]$Lat
  } else {
    longitude[i] = -1
    latitude[i] = -1
  }
}
# Remove rows from ZRI, longtitude, latitude and zipcode that contain zipcode without lat and long
RTR = which(latitude == -1)
ZRI = ZRI[-RTR]
longitude = longitude[-RTR]
latitude = latitude[-RTR]
zipcode = zipcode[-RTR]
newData = cbind(ZRI,zipcode,longitude,latitude)
# At this step we have clean data with 15913 entries
boxplot(ZRI,xlab = "ZRI", main = "Box plot of Zillow rental index")

# From this graph we can see there's huge gap between data, which is reasonalbe because
# -160 longitude represnts the hawaii
plot(longitude,ZRI, main = "ZRI vs longitude")

plot(latitude, ZRI, main = "ZRI vs latitude")

# Take out data from alask and hawaii
RTRlati = which(latitude >50)
RTRlong = which(longitude < -125)
newData = newData[-c(RTRlati,RTRlong),]

plot(newData[,3],newData[,1],main = "ZRI vs longitude", xlab = "longitude", ylab = "ZRI")
plot(newData[,4],newData[,1],main = "ZRI vs latitude", xlab = "latitude", ylab = "ZRI")

RTKlong = which(newData[,3] < -73.50 & newData[,3] > -80.94)
TTemp = newData[RTKlong,]
# Filter out the row by new defined latitude
RTKlati = which(TTemp[,4] > 37.57 & TTemp[,4] < 40.75)
SData = TTemp[RTKlati,]
SData
#write.table(SData, "C:/Users/subei_000/Desktop/404 Project/smallData.txt", row.names=FALSE)

# Read out come data obtained from matlab
library(openxlsx)
mydf <- read.xlsx("FFinalData.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
head(mydf)
#plot
library(RCurl)
library(ggmap)
library(ggplot2)
names(mydf) = c('ZRI','Longitude','Latitude')
map<-get_map(location=c(-80.94,37.57,-73.50,40.75), zoom=7, maptype = "terrain",
             source='google',color='color')
ggmap(map)
ggmap(map) + geom_point(
  aes(x=Longitude, y=Latitude, colour=ZRI), 
  data=mydf, alpha=.2, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")


