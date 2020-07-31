library(raster) 
library(dismo)
library(rJava) 
library(rgdal)
library(rgbif)
library(sp)
library(foreach)
library(pryr)
library(maptools)
library(doParallel)
library(sf)
library(xlsx)
options(stringsAsFactors = FALSE)
extend <- c(-85,-68,-35,-1) #long, long , lat, lat (lado suroeste, lado noreste) para gbif
sp="Tyrannus tyrannus"
GBIFdata <- occ_data(scientificName=sp, hasCoordinate=TRUE, geometry=extend,
                     hasGeospatialIssue=FALSE,limit = 5000)

data(wrld_simpl)
plot(wrld_simpl, col = "white", axes = T)
area <- as(extent(extend), "SpatialPolygons")
plot(area,alpha= 0.2, add=TRUE)

ocs = as.data.frame(GBIFdata[["data"]])
occs = ocs[,3:4]
occs <- occs[, c(2,1)]
colnames(occs) <- c('Longitude', 'Latitude')

year <- GBIFdata[["data"]][["year"]]
month <- GBIFdata[["data"]][["month"]]
day <- GBIFdata[["data"]][["day"]]
date1 <- (cbind(as.numeric(year),as.numeric(month),as.numeric(day)))
time <- within(as.data.frame(date1), date <- paste(year,month,day,sep='-'))

new_data <- cbind(time,occs)
colnames(new_data)[c(,1:3)] <- c("year","month","day")
library(lubridate)
new_data$date <- ymd(as.character(new_data$date))
library(dplyr)
data <- as_tibble(new_data)
data %>% arrange(date)

data$days <- as.POSIXlt(data$date, format = "%d.%m.%y")$yday


data19 <- subset(data, data$V1=="2019")
data18 <- subset(data, data$V1=="2018")
data17 <- subset(data, data$V1=="2017")
data16 <- subset(data, data$V1=="2016")
data15 <- subset(data, data$V1=="2015")
data14 <- subset(data, data$V1=="2014")
data13 <- subset(data, data$V1=="2013")
data12 <- subset(data, data$V1=="2012")
data11 <- subset(data, data$V1=="2011")
data10 <- subset(data, data$V1=="2010")

ggplot() +
  geom_line(data=data19, aes(x=date, y= Latitude, color="2019"))+
  geom_line(data=data18, aes(x=date, y= Latitude, color="2018"))+
  geom_line(data=data17, aes(x=date, y= Latitude, color="2017"))+
  geom_line(data=data16, aes(x=date, y= Latitude, color="2016"))+
  geom_line(data=data15, aes(x=date, y= Latitude, color="2015"))+
  geom_line(data=data14, aes(x=date, y= Latitude, color="2014"))+
  geom_line(data=data13, aes(x=date, y= Latitude, color="2013"))+
  geom_line(data=data12, aes(x=date, y= Latitude, color="2012"))+
  geom_line(data=data11, aes(x=date, y= Latitude, color="2011"))+
  geom_line(data=data10, aes(x=date, y= Latitude, color="2010"))+
  labs(x="Date", y="Latitude", color="Years")+
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c('1/1/2010', '1/1/2020'), format="%d/%m/%Y"),
               date_labels="%Y")+
  geom_hline(yintercept = -26)+
  geom_hline(yintercept = -35)+
  theme_test()

data09 <- subset(data, data$V1=="2009")
data08 <- subset(data, data$V1=="2008")
data07 <- subset(data, data$V1=="2007")
data06 <- subset(data, data$V1=="2006")
data05 <- subset(data, data$V1=="2005")
data04 <- subset(data, data$V1=="2004")
data03 <- subset(data, data$V1=="2003")
data02 <- subset(data, data$V1=="2002")
data01 <- subset(data, data$V1=="2001")
data00 <- subset(data, data$V1=="2000")

ggplot() +
  geom_line(data=data09, aes(x=date, y= Latitude, color="2009"))+
  geom_line(data=data08, aes(x=date, y= Latitude, color="2008"))+
  geom_line(data=data07, aes(x=date, y= Latitude, color="2007"))+
  geom_line(data=data06, aes(x=date, y= Latitude, color="2006"))+
  geom_line(data=data05, aes(x=date, y= Latitude, color="2005"))+
  geom_line(data=data04, aes(x=date, y= Latitude, color="2004"))+
  geom_line(data=data03, aes(x=date, y= Latitude, color="2003"))+
  geom_line(data=data02, aes(x=date, y= Latitude, color="2002"))+
  geom_line(data=data01, aes(x=date, y= Latitude, color="2001"))+
  geom_line(data=data00, aes(x=date, y= Latitude, color="2000"))+
  labs(x="Date", y="Latitude", color="Years")+
  scale_x_date(date_breaks = "1 year", 
               limits = as.Date(c('1/1/2000', '1/1/2009'), format="%d/%m/%Y"),
               date_labels="%Y")+
  geom_hline(yintercept = -26)+
  geom_hline(yintercept = -35)+
  theme_test()
  
ggplot() +
  geom_line(data=data19, aes(x=days, y= Latitude, color="2019"))+
  geom_line(data=data18, aes(x=days, y= Latitude, color="2018"))+
  geom_line(data=data17, aes(x=days, y= Latitude, color="2017"))+
  geom_line(data=data16, aes(x=days, y= Latitude, color="2016"))+
  geom_line(data=data15, aes(x=days, y= Latitude, color="2015"))+
  geom_line(data=data14, aes(x=days, y= Latitude, color="2014"))+
  geom_line(data=data13, aes(x=days, y= Latitude, color="2013"))+
  geom_line(data=data12, aes(x=days, y= Latitude, color="2012"))+
  geom_line(data=data11, aes(x=days, y= Latitude, color="2011"))+
  geom_line(data=data10, aes(x=days, y= Latitude, color="2010"))
        
library(xlsx)
write.xlsx(as.data.frame(occs), "D:/imagenes/datagbif_ttyrannus.xlsx")
