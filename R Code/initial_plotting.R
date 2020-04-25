# libraries
library(readr)
library(fields)
library(rworldmap)

#### Data Acquisition ####
# getting insigth from them on econ indicators they'd want for econd
# they might have ideas on coming up w/ correlations
# look at both definitions of extreme
# use read_delim; skip lines as specified in .csv files
# col_types = cols() to suppress output specifying types of columns
TOAR_data_95_99 <- read_delim("data/TOAR_sfc_ozone_annual_global_1995-1999_aggregated.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())
TOAR_data_00_04 <- read_delim("data/TOAR_sfc_ozone_annual_global_2000-2004_aggregated.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())
TOAR_data_05_09 <- read_delim("data/TOAR_sfc_ozone_annual_global_2005-2009_aggregated.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())
TOAR_data_08_15 <- read_delim("data/TOAR_sfc_ozone_annual_global_2008-2015_aggregated.csv", 
                              delim = ";", escape_double = FALSE, skip = 50, trim_ws = TRUE, col_types = cols())
TOAR_data_10_14 <- read_delim("data/TOAR_sfc_ozone_annual_global_2010-2014_aggregated.csv", 
                              delim = ";", escape_double = FALSE, skip = 79, trim_ws = TRUE, col_types = cols())

# put data into seperate dataframe so we can make changes without having to reload data every time
TOAR_df_95_99 <- data.frame(TOAR_data_95_99)
TOAR_df_00_04 <- data.frame(TOAR_data_00_04)
TOAR_df_05_09 <- data.frame(TOAR_data_05_09)
TOAR_df_08_15 <- data.frame(TOAR_data_08_15)
TOAR_df_10_14 <- data.frame(TOAR_data_10_14)

#### Data Exploration ####
## Create plots for each 4-year chunk

# List of years as well as the data itself
# don't use 2008-15 right now because it doesn't have 99th percentile; does have 98th
TOAR_data_frames <- list(TOAR_df_95_99, TOAR_df_00_04, TOAR_df_05_09, TOAR_df_10_14)
year_names <- list('1995-99', '2000-04', '2005-09', '2010-14')

# have consisten z-lim by getting max/min of all 99th percentiles accross all dfs
# used when plotting globally to tell trends accross years; not used when plotting sub-regions
zr_global <- range(c(TOAR_df_95_99[which(TOAR_df_95_99$p99 != -999),]$p99,
                     +         TOAR_df_00_04[which(TOAR_df_00_04$p99 != -999),]$p99,
                     +         TOAR_df_05_09[which(TOAR_df_05_09$p99 != -999),]$p99,
                     +         TOAR_df_10_14[which(TOAR_df_10_14$p99 != -999),]$p99))

# plot parameters good for printing
par(mar=c(1.5,1,3,1))
par(cex.main=1.75)

# fix params if we want to view in terminal
# dev.off()

for (i in seq(1,4)){
  # get data as well as label
  df <- TOAR_data_frames[[i]] # [[i]] because list of dfs behaves strangely
  year_label <- year_names[i]
  print(year_label)

  # remove NA & data without 99th percentile reading (-999)
  df <- df[complete.cases(df),]
  df <- df[which(df$p99 != -999),]

  # plot the whole world events
  quilt.plot(df[,c('station_lon', 'station_lat')], df$p99, nx=150, ny=100, zlim=zr_global,
             main=paste('99th Percentile Ozone Concentrations Globally [nmol/mol], ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/global_', year_label, '.png', sep=''), width = 1800, height = 1200)

  # and then also display the log population density
  # without the log transform data isn't all that useful
  df$log_pop_density <- 0
  non_zero_ind <- which(df$station_population_density != 0) # make sure we're not taking log(0)
  df[non_zero_ind,]$log_pop_density <- log(df[non_zero_ind,]$station_population_density)

  quilt.plot(df[which(df$station_lat > 0 & df$station_lat < 60),c('station_lon', 'station_lat')],
             df[which(df$station_lat > 0 & df$station_lat < 60),]$log_pop_density, nx=400, ny=100,
             main=paste('Log-transformed Population Density, ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/population_', year_label, '.png', sep=''), width = 2000, height=500)

  # and then the night lights
  quilt.plot(df[which(df$station_lat > 0 & df$station_lat < 60),c('station_lon', 'station_lat')],
             df[which(df$station_lat > 0 & df$station_lat < 60),]$station_nightlight_1km, nx=400, ny=100,
             main=paste('Nighttime Light Intensity index, ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/lights_', year_label, '.png', sep=''), width = 2000, height=500)

  # set z-lim for ozone relative to max/min that year
  zr <- range(df$p99)

  # and then break it up into North America
  TOAR_na <- df[which(df$station_lon > -130 & df$station_lon < -50 &
                           df$station_lat > 0 & df$station_lat < 60),]
  quilt.plot(TOAR_na[,c('station_lon', 'station_lat')], TOAR_na$p99, nx=75, ny=75, zlim=zr,
             main=paste('99th Percentile Ozone Concentrations in North America [ppb], ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/na_', year_label, '.png', sep=''), width = 1200, height = 1200)


  # Europe
  TOAR_eu <- df[which(df$station_lon > -10 & df$station_lon < 30 &
                             df$station_lat > 30 & df$station_lat < 65),]
  quilt.plot(TOAR_eu[,c('station_lon', 'station_lat')], TOAR_eu$p99, nx=75, ny=75, zlim=zr,
             main=paste('99th Percentile Ozone Concentrations in Europe [ppb], ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/eu_', year_label, '.png', sep=''), width = 1200, height = 1200)

  # and Asia
  TOAR_asia <- df[which(df$station_lon > 110 & df$station_lon < 150 &
                             df$station_lat > 20 & df$station_lat < 60),]
  quilt.plot(TOAR_asia[,c('station_lon', 'station_lat')], TOAR_asia$p99, nx=75, ny=75, zlim=zr,
             main=paste('99th Percentile Ozone Concentrations in Eastern Asia [ppb], ', year_label))
  map(add=TRUE)
  dev.print(png, paste('plots/Initial Plots/asia_', year_label, '.png', sep=''), width = 1200, height = 1200)
}
