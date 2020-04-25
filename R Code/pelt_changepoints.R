library(readr)
library(fields)
library(rworldmap)
library(ggplot2)
library(gganimate)
library(reshape2)
library(gifski) # depends on PNG!
library(zoo)
library(changepoint)

TOAR_sfc_summer <- read_delim("C:/Users/Baylee/Dropbox/OzoneExtremes/data/TOAR_sfc_ozone_summer_global_1970-2014.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

valid_dma8 <- TOAR_sfc_summer[which(TOAR_sfc_summer$avgdma8epax != -999),]
dma8_id_table <- table(valid_dma8$station_id)
dma8_id_table <- dma8_id_table[dma8_id_table > 30]
dma8_id_valid <- as.data.frame(dma8_id_table)$Var1
valid_dma8 <- valid_dma8[which(valid_dma8$station_id %in% dma8_id_valid),]

stations <- unique(valid_dma8$station_id)

valid_dma8$change_year <- 0

count = 0
change_points_data <- data.frame(station_id = character(),
                                 station_lat = double(),
                                 station_lon = double(),
                                 change_year = double(),
                                 stringsAsFactors = FALSE)

for(station in stations){
  count = count + 1
  # get station data
  station_df <- valid_dma8[which(valid_dma8$station_id == station),]
  start_year <- as.numeric(substring(min(station_df$datetime), 0, 4))
  station_data <- ts(station_df$avgdma8epax, start=start_year)
  lat_lon <- unique(valid_dma8[which(valid_dma8$station_id == station),c('station_lat', 'station_lon')])
  
  # do changepoint things, PELT 
  station.mean <- cpt.mean(station_data, method = 'PELT')
  #plot(station.mean)
  
  # store the start year in df
  change_points <- cpts(station.mean) + start_year
  print(change_points)
  change_amounts <- coef(station.mean)$mean

  # check to see if there are any change points
  if(length(change_points) > 0){
    # add them all to the df
    for (i in 1:length(change_points)){
      change_points_data[nrow(change_points_data)+1,] <- c(station, lat_lon, change_points[i], change_amounts[i+1] - change_amounts[i])
    }
  }
  # if we want to track stations w/o change points
  else{
    #change_points_data[nrow(change_points_data)+1,] <- c(station, lat_lon, -1, -1)
  }
}

# histogram showing number of changes and total change points
# create a frequency table
change_years <- table(change_points_data$change_year)
yearly_stations <- table(as.double(format(as.Date(valid_dma8$datetime), '%Y')))
percent_changes <- data.frame(change_years / yearly_stations[5:44])

# axis labels like this because I don't know
brk <- rep('',4)
axis_lab <- c(1975, brk, 1980, brk, 1985, brk, 1990, brk, 1995, brk, 2000, brk, 2005, brk, 2010, rep('',4))

# and then create the plot
g <- ggplot(percent_changes, aes(Var1, Freq)) + 
  geom_bar(stat='identity') +
  scale_x_discrete(labels=axis_lab) +
  labs(x='Year', y='Percent of stations with changepoint', title='Percent of stations with PELT Changepoints, 1975-2013') +
  theme(text=element_text(size=16))

plot(g)

#Create new Count and DF
count = 0
change_points_data2 <- data.frame(station_id = character(),
                                 station_lat = double(),
                                 station_lon = double(),
                                 change_amounts = double(),
                                 change_year = double(),
                                 country = character(),
                                 stringsAsFactors = FALSE)


#Find Direction of Change Point Using AMOC
for(station in stations){
  count = count + 1
  # get station data
  station_df <- valid_dma8[which(valid_dma8$station_id == station),]
  start_year <- as.numeric(substring(min(station_df$datetime), 0, 4))
  station_data <- ts(station_df$avgdma8epax, start=start_year)
  lat_lon <- unique(valid_dma8[which(valid_dma8$station_id == station),c('station_lat', 'station_lon')])
  country <- station_df$station_country
  
  # do changepoint things, AMOC 
  station.mean <- cpt.mean(station_data, method = 'AMOC')
  #plot(station.mean)
  
  # store the start year in df
  change_points <- cpts(station.mean) + start_year
  print(change_points)
  change_amounts <- coef(station.mean)$mean
  
  # check to see if there are any change points
  if(length(change_points) > 0){
    # add them all to the df
    for (i in 1:length(change_points)){
      change_points_data2[nrow(change_points_data2)+1,] <- c(station, lat_lon, change_amounts[i+1] - change_amounts[i],change_points,country)
    }
  }
  else{
    change_points_data2[nrow(change_points_data2)+1,] <- c(station, lat_lon, -1)
  }
 }


#Get rid of stations with no change points 
change_points_data2 <- change_points_data2[which(change_points_data2$change_amounts != -1),]
#Initialize direction column
change_points_data2$direction <- 0

#Determine direction of change point and save in DF
  #1 is increase in mean and -1 is decrease in mean
positive = 0 
negative = 0
for (row in 1:nrow(change_points_data2)) {
  if(change_points_data2$change_amounts[row] > 0){
    change_points_data2$direction[row] <- 1
    positive = positive + 1
  }
  else{
    change_points_data2$direction[row] <- -1
    negative = negative + 1
  }
}
#Create Quilt Plot of US Direction of Change Points
US_stations <- change_points_data2[which(change_points_data2$station_lon > -130 & change_points_data2$station_lon < -50 &
                      change_points_data2$station_lat > 0 & change_points_data2$station_lat < 60),]
quilt.plot(US_stations[c('station_lon', 'station_lat')], US_stations$direction,
           main='Direction of Change Point of Mean Direction US',nlevel=2,col = tim.colors(2))
map(add=TRUE)
US(add=TRUE)

#Create Histogram of Change Point Direction
hist(change_points_data2$direction,main='Histogram of Change Point of Mean Direction',xlab='Change Point Direction')
positive = 0
negative = 0
for (i in 1:nrow(US_stations)){
  if (US_stations$direction[i] == 1){
    positive = positive + 1
  }
  else {
    negative = negative + 1
  }
}
positive
negative

unique(change_points_data2$country)
#Create DF With just european data
europe_data <- change_points_data2[which(change_points_data2$country == "Germany" | change_points_data2$country == "United Kingdom"),]
#Create DF with just North American Data
northamerica_data <- change_points_data2[which(change_points_data2$country == "United States of America" | change_points_data2$country == "Canada"),]
#Create DF With Asian Data
asia_data <- change_points_data2[which(change_points_data2$country == "Japan"),]

europe_data$change_year <- as.numeric(europe_data$change_year)
northamerica_data$change_year <- as.numeric(northamerica_data$change_year)
asia_data$change_year <- as.numeric(asia_data$change_year)

#Histogram of Change Years Split Between Regions
#Europe
hist(europe_data$change_year,breaks=seq(1970,2018),main='Histogram of Change Point Years in Europe',xlab='Year of Change Point')
#Asia
hist(asia_data$change_year,breaks=seq(1970,2018),main='Histogram of Change Point Years in Asia',xlab='Year of Change Point')
#North America
hist(northamerica_data$change_year,breaks=seq(1970,2018),main='Histogram of Change Point Years North America',xlab='Year of Change Point')

quilt.plot(northamerica_data[c('station_lon', 'station_lat')], northamerica_data$change_year,
           main='Year of Change Point of Mean Direction US')
map(add=TRUE)
US(add=TRUE)

quilt.plot(US_stations[c('station_lon', 'station_lat')], as.numeric(US_stations$change_year),
           main='Direction of Change Point of Mean Direction US')
map(add=TRUE)
US(add=TRUE)

ohio_stations <- change_points_data2[which(change_points_data2$station_lon > -85 & change_points_data2$station_lon < -80 &
                                           change_points_data2$station_lat > 38 & change_points_data2$station_lat < 42),]
quilt.plot(ohio_stations[c('station_lon', 'station_lat')], as.numeric(ohio_stations$change_year))
map(add=TRUE)
US(add=TRUE)
#Ohio Data Histogram
hist(as.numeric(ohio_stations$change_year),breaks=seq(1970,2018),main='Year of Change Points in Ohio',xlab='Year')

quilt.plot(asia_data[c('station_lon', 'station_lat')],asia_data$direction,nlevel=2,col = tim.colors(2))
map(add=TRUE)

hist(ohio_stations$direction)
quilt.plot(ohio_stations[c('station_lon', 'station_lat')],ohio_stations$direction,nlevel=2,col = tim.colors(2))
map(add=TRUE)
US(add=TRUE)

east_coast <- change_points_data2[which(change_points_data2$station_lon > -85 & change_points_data2$station_lon < -50 &
                                            change_points_data2$station_lat > 0 & change_points_data2$station_lat < 60),]
quilt.plot(east_coast[c('station_lon', 'station_lat')],east_coast$direction,nlevel=2,col = tim.colors(2))
map(add=TRUE)
US(add=TRUE)

east_coast$direction

positive=0
negative=0
for (i in 1:nrow(east_coast)){
  if (east_coast$direction[i] == 1){
    positive = positive + 1
  }
  else {
    negative = negative + 1
  }
}

positive
negative

new <- east_coast[which(east_coast$change_year >2005)]

hist(east_coast$change_year)

plot(east_coast$change_year,east_coast$change_amounts)

post_data <- east_coast[which(east_coast$change_year > 2005),]
hist(post_data$change_year)

positive=0
negative=0
for (i in 1:nrow(post_data)){
  if (post_data$direction[i] == 1){
    positive = positive + 1
  }
  else {
    negative = negative + 1
  }
}

positive
negative

g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = post_data, mapping = aes(station_lon, station_lat, color=direction), size=2) + 
  scale_color_gradientn(colors=c('blue', 'white', 'red'), name='Magnitude of \nchange (ppb)', limits=c(-2,2)) +
  labs(title='Magnitude of dma8 Change Points in the United States during the 1990s') +
  theme_void(base_size=20)
plot(g)

# create column for positive/negative
post_data['is_positive'] <- 'Decrease'
post_data[which(post_data$change_amount > 0),'is_positive'] <- 'Increase'



g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = post_data, mapping = aes(station_lon, station_lat, color=is_positive), size=2) + 
  scale_color_manual(values=c("Decrease" = "#55BF2E", "Increase" = "Red")) +
  labs(title='Direction of dma8 Change Points in the United States', color='Mean Change Direction') +
  theme_void(base_size=20)

plot(g)



