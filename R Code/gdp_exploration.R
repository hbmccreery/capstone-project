# libraries
library(readr)
library(ggplot2)
library(gganimate)
library(reshape2)
library(gifski) # depends on PNG!
library(zoo)

#### Data Acquisition ####
# getting insigth from them on econ indicators they'd want for econd
# they might have ideas on coming up w/ correlations
# look at both definitions of extreme
# use read_delim; skip lines as specified in .csv files
# col_types = cols() to suppress output specifying types of columns

TOAR_sfc_annual <- read_delim("data/TOAR_sfc_ozone_annual_global_1970-2014.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

TOAR_sfc_summer <- read_delim("data/TOAR_sfc_ozone_summer_global_1970-2014.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

#### GDP Data processing ####
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

#### GDP vs Pollution plots #### 
p <- ggplot(data_total[which(data_total['Country Name'] != 'Mexico'),],
            aes(x=log(`Real GDP`), y=avgdma8epax)) + 
  geom_point() + 
  labs(title = 'Log-scaled GDP and National Average Daily 8-hour Max Pollution Levels, 1971-2014',
       y = 'Daily maximum 8-hour ozone mixing ratio (nmol mol-1)',
       x = 'Log-scaled Real GDP (current USD)') +
  theme_grey(base_size = 16)

plot(p)

p <- ggplot(data_total[which(data_total['Country Name'] != 'Mexico'),],
            aes(x=log(`Real GDP`), y=nvgt070)) + 
  geom_point() + 
  labs(title = 'Log-scaled GDP and National Average Days with Extreme Ozone Levels, 1971-2014',
       y = 'Days in exceedance of 70 ppb Ozone guideline',
       x = 'Log-scaled Real GDP (current USD)') +
  theme_grey(base_size = 16)

plot(p)

#### Animated plots ####
# this takes awhile
# p <- ggplot(data_total, aes(x=nvgt070, y=log(`Real GDP`))) +
#   geom_point() +
#   transition_states(Year,
#                     transition_length = 2,
#                     state_length = 2) +
#   labs(title = 'Year: {closest_state}')
# 
# animate(p)

#### Basic Linear Model ####
# get only the EU countries
EU_countries <- unique(data_total$`Country Name`)[c(3,5,8,9,10,11,12,13,15,16,19,20,21,22,24,25,26,27)]

data_EU <- data_total[which(data_total$`Country Name` %in% EU_countries),]

pairs(data_EU[2:6])
model <- lm(nvgt070 ~ `Real GDP` + Year, data = data_EU)

#### Country-level time series ####
us <- data_total[which(data_total$`Country Name` == 'United States of America'),]
japan <- data_total[which(data_total$`Country Name` == 'Japan'),]

countries <- c('United States of America', 'Japan', 'Spain', 'Germany')
# if you want it for all countries
# countries <- unique(data_total$`Country Name`)

# it looks like this still works but I can't really tell?
for(country in countries){
  subset <- data_total[which(data_total$`Country Name` == country),]
  
  if(country == 'United States of America'){country <- 'USA'}
  
  factor <- max(subset$`Real GDP`/(10^12)) / max(rollmean(subset$nvgt070, 5, fill=0))
  start_year <- min(subset$Year)
  title = paste('GDP and pollution growth in ', country, ', ', start_year, '-2014', sep='')
  
  p <- ggplot(subset, aes(x=Year)) +
    geom_line(aes(y=`Real GDP`/(10^12), color='GDP')) +
    geom_point(aes(y=`Real GDP`/(10^12), color='GDP')) +
    geom_line(aes(y=rollmean(nvgt070*factor, 5, fill=NA), color='nvgt070 MA')) +
    geom_point(aes(y=rollmean(nvgt070*factor, 5, fill=NA), color='nvgt070 MA')) +
    labs(title=title, y='GDP (Trillions of USD)', color='color') +
    scale_y_continuous(sec.axis = sec_axis(~./factor, name = "Days above 70 ppb")) +
    scale_colour_manual(values = c("blue", "red")) +
    theme_gray(base_size=24) +
    guides(col = guide_legend(override.aes = list(shape = 15, size = 10)))
  
  filename <- paste('plots/country_pollution/', country, '_nvgt.png', sep='')
  ggsave(filename, plot=p, device='png')
  
  factor <- max(subset$`Real GDP`/(10^12)) / max(rollmean(subset$avgdma8epax, 5, fill=0))
  start_year <- min(subset$Year)
  title = paste('GDP and pollution growth in ', country, ', ', start_year, '-2014', sep='')
  
  p <- ggplot(subset, aes(x=Year)) +
    geom_line(aes(y=`Real GDP`/(10^12), color='GDP')) +
    geom_point(aes(y=`Real GDP`/(10^12), color='GDP')) +
    geom_line(aes(y=rollmean(avgdma8epax*factor, 5, fill=NA), color='dma8 MA')) +
    geom_point(aes(y=rollmean(avgdma8epax*factor, 5, fill=NA), color='dma8 MA')) +
    labs(title=title, y='GDP (Trillions of USD)', color='color') +
    scale_y_continuous(sec.axis = sec_axis(~./factor, name = "National Daily 8-hour Max Pollution")) +
    scale_colour_manual(values = c("red", "blue")) +
    theme_gray(base_size=24) +
    guides(col = guide_legend(override.aes = list(shape = 15, size = 10)))
  
  filename <- paste('plots/country_pollution/', country, '_dma8.png', sep='')
  ggsave(filename, plot=p, device='png')
  
  title = paste('Relationship between GDP and pollution levels in ', country, ', ', start_year, '-2014', sep='')
  p <- ggplot(subset, aes(x=`Real GDP`, y=nvgt070)) +
    geom_point() +
    labs(title=title) +
    theme_gray(base_size=24)
  
  plot(p)
}