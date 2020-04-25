# libraries
library(readr)
library(fields)
library(rworldmap)
library(ggplot2)
library(gganimate)
library(reshape2)
library(gifski) # depends on PNG!
library(zoo)
library(changepoint)

#### Data Acquisition ##########################
# use read_delim; skip lines as specified in .csv files
# col_types = cols() to suppress output specifying types of columns
# get both the annual and summertime data for now

TOAR_sfc_annual <- read_delim("data/TOAR_sfc_ozone_annual_global_1970-2014.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

TOAR_sfc_summer <- read_delim("data/TOAR_sfc_ozone_summer_global_1970-2014.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

#### GDP Data processing ##########################
## DATA NOT USED IN FINAL REPORT
# get GDP stuff
gdp_data <- read_csv('data/country_level_gdp.csv', skip=4)

# break GDP data apart so that each year is separate record
gdp_melted <- melt(gdp_data, id=c('Country Name', 'Country Code', 'Indicator Name', 'Indicator Code'),
                   value.name = 'Real GDP', na.rm = TRUE)

# similar process for TOAR data
toar_melted <- melt(TOAR_sfc_summer, id.vars = c('station_country', 'datetime'), 
                    measure.vars = c('avgdma8epax', 'nvgt070', 'p99'))
# remove invalid records
toar_melted <- toar_melted[which(toar_melted$value != -999),]

# cast to aggregate metrics
toar_casted <- dcast(toar_melted, station_country + datetime ~ variable, mean)

# convert datetime -> year to join w/ GDP
years <- lapply(toar_casted[2], function(x) as.double(substr(x,1,4)))
toar_casted['Year'] <- years[1]

# few naming issues
gdp_melted[which(gdp_melted$`Country Name` == 'United States'),'Country Name'] <- 'United States of America'
gdp_melted[which(gdp_melted$`Country Name` == 'Russian Federation'),'Country Name'] <- 'Russia'

colnames(toar_casted)[1] <- 'Country Name'
colnames(gdp_melted)[5] <- 'Year'

# put them together, get rid of NAs
merged_df <- merge(toar_casted, gdp_melted, on=c('Country Name', 'Year'))
data_total <- merged_df[complete.cases(merged_df),]

# the GDP data gets turned into strings with the melt, so undo that
data_total$`Real GDP` <- as.double(data_total$`Real GDP`)

# also don't care about a bunch of the stuff that gets brought with the GDP data
data_total <- data_total[-c(3,7,8,9)]

#### Change Points ##########################
# drop any summer points without valid data
valid_dma8 <- TOAR_sfc_summer[which(TOAR_sfc_summer$avgdma8epax != -999),]

# find stations with more than 30 years by creating table with frequencies of
# apperances in the summer data
dma8_id_table <- table(valid_dma8$station_id)
dma8_id_table <- dma8_id_table[dma8_id_table > 30]
dma8_id_valid <- as.data.frame(dma8_id_table)$Var1

# filter our valid data for more than 30 years
valid_dma8 <- valid_dma8[which(valid_dma8$station_id %in% dma8_id_valid),]

# create df to store change point information about
change_points_data <- data.frame(station_id = character(),
                                 station_lat = double(),
                                 station_lon = double(),
                                 change_year = double(),
                                 change_amount = double(),
                                 stringsAsFactors = FALSE)

# define asymptotic penalty to be used in the AMOC method
pen.value <- 0.05

# list of stations to iterate on
stations <- unique(valid_dma8$station_id)

# switch used to save time series plots of significant points
save_plots <- FALSE

# find change points for all stations
for(station in stations){
  # get station data
  station_df <- valid_dma8[which(valid_dma8$station_id == station),]
  start_year <- as.numeric(substring(min(as.Date(station_df$datetime)), 0, 4))
  station_data <- ts(station_df$avgdma8epax, start=start_year)
  
  # get lat/lon info for station
  lat_lon <- unique(valid_dma8[which(valid_dma8$station_id == station),c('station_lat', 'station_lon')])
  
  # do changepoint things, AMOC at 1-pen.value level
  station.mean <- cpt.mean(station_data, method = 'AMOC', penalty="Asymptotic", pen.value=pen.value)

  # store the start year in df
  change_points <- cpts(station.mean) + start_year

  # also interested in direction/magnitude of change
  change_amount <- coef(station.mean)$mean[2] - coef(station.mean)$mean[1]
  
  # if there was a valid change, add it to our df
  if(length(change_points) > 0){
    change_points_data[nrow(change_points_data)+1,] <- c(station, lat_lon, change_points, change_amount)
    
    # plot the series in the SoCal cluster if switch is on
    if(((change_amount < -17 & change_points > 1985) | change_amount < -20) & save_plots == TRUE){
      plot(station.mean, main=station, ylab='Mean dma8 level', xlab='Year')
      filename <- paste('plots/time series/california cluster/', substr(station,1,11), '.png', sep='')
      dev.print(png, filename, width=1200, height=550)
    }
  }
  
  # if no valid change, add a point to indicate an error
  else{
    change_points_data[nrow(change_points_data)+1,] <- c(station, lat_lon, -1, -1)
    # plot the series without change if switch is on
    if(save_plots == TRUE){
      plot(station.mean, main=station, ylab='Mean dma8 level', xlab='Year')
      filename <- paste('plots/time series/', substr(station,1,11), '.png', sep='')
      dev.print(png, filename, width=1200, height=550)
    }
  }
  
  if(station == 'SMO514S00'){
    plot(station.mean, main='No Change in Mean dma8 Levels', ylab='Mean dma8 level', xlab='Year')
  }
  if(station == '06-037-1701'){
    plot(station.mean, main='Yearly dma8 Levels for Southern California Station', ylab='Mean dma8 level', xlab='Year')
  }
}

#### Change Point Exploration ##########################
# plot change points for all stations
# these all follow the pattern of subset/plot/outlines
# they all also look awful because they're quilt plots
valid_changes <- change_points_data[which(change_points_data$change_year > 0),]
quilt.plot(valid_changes[c('station_lon', 'station_lat')], valid_changes$change_year,
           main='dma8 Change Points Globally')
map(add=TRUE)

na_changes <- valid_changes[which(valid_changes$station_lon < -50 & valid_changes$station_lon > -140),]
quilt.plot(na_changes[c('station_lon', 'station_lat')], na_changes$change_year,
           main='dma8 Change Points in North America')
map(add=TRUE)
US(add=TRUE)

quilt.plot(na_changes[c('station_lon', 'station_lat')], na_changes$change_amount,
           main='Magnitude of dma8 Change Points in North America')
map(add=TRUE)
US(add=TRUE)

jp_changes <- valid_changes[which(valid_changes$station_lon > 100 & valid_changes$station_lat > 0),]
quilt.plot(jp_changes[c('station_lon', 'station_lat')], jp_changes$change_year,
           main='dma8 Change Points in Japan')
map(add=TRUE)

quilt.plot(jp_changes[c('station_lon', 'station_lat')], jp_changes$change_amount,
           main='Magnitude of dma8 Change Points in Japan')
map(add=TRUE)

na_negative <- na_changes[which(na_changes$change_amount < 0),]
quilt.plot(na_negative[c('station_lon', 'station_lat')], na_negative$change_amount,
           main='Magnitude of dma8 Change Points in North America')
map(add=TRUE)
US(add=TRUE)

# create column for if the change is increasing/decreasing
valid_changes['is_positive'] <- 'Decreasing'
valid_changes[which(valid_changes$change_amount > 0),'is_positive'] <- 'Increasing'

# histogram showing number of changes and total change points
# create a frequency table
change_years <- table(valid_changes$change_year)
yearly_stations <- table(as.double(format(as.Date(valid_dma8$datetime), '%Y')))
percent_changes <- data.frame(change_years / yearly_stations[5:43])

# axis labels like this because I don't know
brk <- rep('',4)
axis_lab <- c(1975, brk, 1980, brk, 1985, brk, 1990, brk, 1995, brk, 2000, brk, 2005, brk, 2010, rep('',3))

g <- ggplot(percent_changes, aes(Var1, Freq)) + 
      geom_bar(stat='identity') +
      scale_x_discrete(labels=axis_lab) +
      labs(x='Year', y='Percent of stations with changepoint', title='Percent of stations with AMOC Changepoints, 1975-2013') +
      theme(text=element_text(size=16))
  
plot(g)

# also need to put together a histogram of the total # of stations
g <- ggplot(valid_dma8, aes(as.double(format(as.Date(valid_dma8$datetime), '%Y')))) +
      geom_histogram(binwidth = 1) +
      labs(x='Year', y='Number of Stations', title='Yearly number of stations') +
      theme(text=element_text(size=16))
plot(g)

# histogram which shows number of directional changes at each year
g <- ggplot(valid_changes, aes(change_year, fill = is_positive)) + 
  geom_histogram(binwidth = 1) + 
  labs(title='Global distribution of change points of dma8, AMOC', x='Year', y='Number of change points', fill='Mean Change Direction') +
  scale_fill_manual(values=c("#55BF2E", "Red")) +
  theme_gray(base_size=20)

plot(g)

# create seperate df for only US data because it's what we'll be working on from here on out
valid_us <- valid_changes[which(valid_changes$station_lon < -50 & valid_changes$station_lon > -130),]
valid_us <- valid_us[which(valid_us$station_lat < 48),]

# US map for ggplot
states <- map_data('state')

# define scale for magnitude coloring
color_scale <- c('darkblue', 'blue', 'white', 'red', 'darkred')

# plot magnitude of each station's changes on US map
g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
      coord_fixed(1.3) + 
      geom_point(data = valid_us, mapping = aes(station_lon, station_lat, color=change_amount), size=2) + 
      scale_color_gradientn(colors=color_scale, name='Magnitude of \nchange (ppb)', limits=c(-45,45)) + 
      labs(title='Magnitude of dma8 Change Points in the United States') +
      theme_void(base_size=20)

plot(g)

# plot direction of each station's changes on US map
g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
      coord_fixed(1.3) + 
      geom_point(data = valid_us, mapping = aes(station_lon, station_lat, color=is_positive), size=2) + 
      scale_color_manual(values=c("Decreasing" = "#55BF2E", "Increasing" = "Red")) +
      labs(title='Direction of dma8 Change Points in the United States', color='Mean Change Direction') +
      theme_void(base_size=20)

plot(g)

# save our US AMOC change points to be used in clustering
write.csv(valid_us, file='data/us_change_points.csv', row.names = FALSE)
write.csv(valid_dma8, file='data/valid_dma8.csv', row.names=FALSE)

