library(rgbif)
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(xlsx)

# Search extension for GBIF
extend <- c(-85,-68,-35,0) #sw-lon, ne-lon , sw-lat, ne-lat

# Species scientific name
sp <-'Tyrannus tyrannus'

# GBIF Query
GBIFdata <- occ_data(scientificName=sp, hasCoordinate=TRUE, geometry=extend,hasGeospatialIssue=FALSE,limit = 5000)

# Data transformation from GBIF's data to data.frame
ocurrence <- as.data.frame(GBIFdata[['data']])
ocurrence <- ocurrence[,3:4]
ocurrence <- ocurrence[, c(2,1)]
colnames(ocurrence) <- c('Longitude', 'Latitude')

# Date data transformation
year <- GBIFdata[['data']][['year']]
month <- GBIFdata[['data']][['month']]
day <- GBIFdata[['data']][['day']]

date <- (cbind(as.numeric(year),as.numeric(month),as.numeric(day)))
time <- within(as.data.frame(date), Date <- paste(year,month,day,sep='-'))
ocurrence_time <- cbind(time,ocurrence)
ocurrence_time$Date <- ymd(as.character(ocurrence_time$Date)) #as date

# Rearrange the dataset by date
data <- as_tibble(ocurrence_time)
data %>% arrange(date)

# Import the dataset: ocurrences 
# in the arid ecosystems of western South America
mydata <- read_excel(file.choose())

# Function to convert D.M.S. into decimal degrees
dec.dg <- function(c) {
  dg <- as.character(c)
  x <- do.call(rbind, strsplit(dg, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# Transform the imported dataset
mydata$Latitude <- dec.dg(mydata$Latitude)
mydata$Latitude <- paste0('-',mydata$Latitude)
mydata$Latitude <- as.numeric(mydata$Latitude)
mydata$Date <-as.Date(mydata$Date, format = '%Y/%m/%d')

# Divide the GBIF's dataset by year
# Decade 2010-2019
data19 <- subset(data, data$V1=='2019')
data18 <- subset(data, data$V1=='2018')
data17 <- subset(data, data$V1=='2017')
data16 <- subset(data, data$V1=='2016')
data15 <- subset(data, data$V1=='2015')
data14 <- subset(data, data$V1=='2014')
data13 <- subset(data, data$V1=='2013')
data12 <- subset(data, data$V1=='2012')
data11 <- subset(data, data$V1=='2011')
data10 <- subset(data, data$V1=='2010')

# Decade 2000-2009
data09 <- subset(data, data$V1=='2009')
data08 <- subset(data, data$V1=='2008')
data07 <- subset(data, data$V1=='2007')
data06 <- subset(data, data$V1=='2006')
data05 <- subset(data, data$V1=='2005')
data04 <- subset(data, data$V1=='2004')
data03 <- subset(data, data$V1=='2003')
data02 <- subset(data, data$V1=='2002')
data01 <- subset(data, data$V1=='2001')
data00 <- subset(data, data$V1=='2000')

# Plot Decade 2010-2019
png(file.path('10-19.png'), 
    width=20, height=12, units = 'cm', res = 900)

ggplot() +
  geom_line(data=data19, aes(x=Date, y= Latitude, color='2019'),lwd=0.9)+
  geom_line(data=data18, aes(x=Date, y= Latitude, color='2018'),lwd=0.9)+
  geom_line(data=data17, aes(x=Date, y= Latitude, color='2017'),lwd=0.9)+
  geom_line(data=data16, aes(x=Date, y= Latitude, color='2016'),lwd=0.9)+
  geom_line(data=data15, aes(x=Date, y= Latitude, color='2015'),lwd=0.9)+
  geom_line(data=data14, aes(x=Date, y= Latitude, color='2014'),lwd=0.91)+
  geom_line(data=data13, aes(x=Date, y= Latitude, color='2013'),lwd=0.9)+
  geom_line(data=data12, aes(x=Date, y= Latitude, color='2012'),lwd=0.9)+
  geom_line(data=data11, aes(x=Date, y= Latitude, color='2011'),lwd=0.9)+
  geom_line(data=data10, aes(x=Date, y= Latitude, color='2010'),lwd=0.9)+
  geom_point(data=mydata, aes(x=Date, y=Latitude), size=2.5)+
  labs(x='Year', y='Latitude (°)', color='Year')+
  scale_x_date(date_breaks = '1 year',
               limits = as.Date(c('1/1/2010', '1/1/2020'), 
               format='%d/%m/%Y'),
               date_labels='%Y')+
  geom_hline(yintercept = -26)+
  geom_hline(yintercept = -35)+
  scale_colour_brewer(palette='Spectral')+
  theme_gray(base_size = 15)

dev.off()

# Plot decade 2009-2000
png(file.path('00-09.png'), 
    width=20, height=12, units = 'cm', res = 900)

ggplot() +
  geom_line(data=data09, aes(x=Date, y= Latitude, color='2009'),lwd=0.9)+
  geom_line(data=data08, aes(x=Date, y= Latitude, color='2008'),lwd=0.9)+
  geom_line(data=data07, aes(x=Date, y= Latitude, color='2007'),lwd=0.9)+
  geom_line(data=data06, aes(x=Date, y= Latitude, color='2006'),lwd=0.9)+
  geom_line(data=data05, aes(x=Date, y= Latitude, color='2005'),lwd=0.9)+
  geom_line(data=data04, aes(x=Date, y= Latitude, color='2004'),lwd=0.9)+
  geom_line(data=data03, aes(x=Date, y= Latitude, color='2003'),lwd=0.9)+
  geom_line(data=data02, aes(x=Date, y= Latitude, color='2002'),lwd=0.9)+
  geom_line(data=data01, aes(x=Date, y= Latitude, color='2001'),lwd=0.9)+
  geom_line(data=data00, aes(x=Date, y= Latitude, color='2000'),lwd=0.9)+
  geom_point(data=mydata, aes(x=Date, y=Latitude), size=2.5)+
  labs(x='Year', y='Latitude (°)', color='Years')+
  scale_x_date(date_breaks = '1 year', 
               limits = as.Date(c('1/1/2000', '1/1/2010'), format='%d/%m/%Y'),
               date_labels='%Y')+
  geom_hline(yintercept = -26)+
  geom_hline(yintercept = -35)+
  scale_colour_brewer(palette='Spectral')+
  scale_y_continuous(limits = c(-55,0),breaks = seq(0,-55,-10))+
  theme_gray(base_size = 15)

dev.off()

# Save ocurrences
write.xlsx(as.data.frame(ocurrence), 'ocu_ttyrannus.xlsx')
write.xlsx(as.data.frame(ocurrence_time), 'ocutt_ttyrannus.xlsx')