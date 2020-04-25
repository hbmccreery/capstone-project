# libraries
library(readr)
library(ggplot2)
library(reshape2)
library(changepoint)
library(cluster)
library(factoextra)
library(fda)

#### Data Acquisition ##########################
# read in our AMOC change points, valid dma8 data from capstone_extreme_...
valid_us <- read.csv('data/us_change_points.csv')
valid_dma8 <- read.csv('data/valid_dma8.csv')

# and state information for the plots
states <- map_data('state')

#### Clustering Motivation ##########################
## Plot data pre, during, and post 90's
# define scale for magnitude coloring
color_scale <- c('darkblue', 'blue', 'white', 'red', 'darkred')

# all three of these follow the pattern of subset/plot
# produce plots of magnitude for before the 90s, during 90s, after 90s
valid_us_pre90 <- valid_us[which(valid_us$change_year < 1990),]

g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = valid_us_pre90, mapping = aes(station_lon, station_lat, color=change_amount), size=2) + 
  scale_color_gradientn(colors=color_scale, name='Magnitude of \nchange (ppb)', limits=c(-45,45)) +
  labs(title='Magnitude of dma8 Change Points in the United States before 1990') +
  theme_void(base_size=20)

plot(g)

valid_us_90s <- valid_us[which(valid_us$change_year < 2000 & valid_us$change_year > 1990),]

g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = valid_us_90s, mapping = aes(station_lon, station_lat, color=change_amount), size=2) + 
  scale_color_gradientn(colors=color_scale, name='Magnitude of \nchange (ppb)', limits=c(-45,45)) +
  labs(title='Magnitude of dma8 Change Points in the United States during the 1990s') +
  theme_void(base_size=20)

plot(g)

valid_us_after90 <- valid_us[which(valid_us$change_year >= 2000),]

g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = valid_us_after90, mapping = aes(station_lon, station_lat, color=change_amount), size=2) + 
  scale_color_gradientn(colors=color_scale, name='Magnitude of \nchange (ppb)', limits=c(-45,45)) +
  labs(title='Magnitude of dma8 Change Points in the United States after 2000') +
  theme_void(base_size=20)

plot(g)

# idea is to cluster on amt/year and then examine it spatially
g <- ggplot(valid_us, aes(x=change_year, y=change_amount)) + 
  geom_point() +
  labs(title='Change year and change amount in the United States')

plot(g)

#### K-means Clustering  ##########################
## most of this is just borrowed from https://uc-r.github.io/kmeans_clustering

# Elbow method of picking clusters
cluster_feat <- c('change_year', 'change_amount')#, 'station_lat', 'station_lon')
g <- fviz_nbclust(valid_us[cluster_feat], kmeans, method='wss') +
      geom_vline(xintercept=5, linetype='dashed', color='gray')
plot(g)

# Decide on using 5 clusters; put that together here
set.seed(0)
us_clustered <- kmeans(valid_us[cluster_feat], centers=5, nstart=50)
valid_us['cluster'] <- as.factor(us_clustered$cluster)

# define names, colors for each cluster
cluster_names <- c('Pre-1995 Decrease', 'Post-1995 Decrease', 'Dramatic Decrease', 'Post-1990 Increase', 'Pre-1990 Increase')
cluster_colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

# look at how the clusters behave spatially
g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(data = valid_us, mapping = aes(station_lon, station_lat, color=cluster), size=2) + 
  labs(title='Change Point Cluster Distribution', color='Cluster') +
  scale_color_manual(values=cluster_colors, labels=cluster_names) +
  theme_void(base_size=20)

plot(g) # not a whole lot, but note all the green around LA

# go back to the amt/year plot, this time color in points with the cluster
g <- ggplot(valid_us, aes(x=change_year, y=change_amount, color=cluster)) + 
      geom_point() +
      scale_color_manual(values=cluster_colors, labels=cluster_names) +
      labs(title='K-means clustering on change year and change amount') + 
      xlab('Year of change') + ylab('Change Amount (dma8)')

plot(g) # see that it mainly splits into positive/negative, plus the green ones ~1995

# create spatial plots for each cluster individually
for(i in 1:5){
  # partition into cluster
  us_partition <- valid_us[which(valid_us$cluster == i),]
  
  # plot w/ coloring being the year that it changed
  g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
    coord_fixed(1.3) + 
    geom_point(data = us_partition, mapping = aes(station_lon, station_lat), size=2, color=cluster_colors[i]) +
    #scale_color_gradientn(name='Change  \n Year', colors=c('red', 'white', 'blue'), limits=c(1975, 2015)) +
    labs(title=paste('Spatial distribution of', cluster_names[i], 'cluster')) +
    theme_void(base_size=20)
  
  # toggle ggsave/plot(g) depending on if you want to dl plots
  filename <- paste('plots/cluster distribution/spatial_cluster_', i, '.png', sep='')
  ggsave(filename, plot=g, width=12, height=6, units='in')
  plot(g)
  
  # also create histograms to see behaviour of year in each cluster
  g <- ggplot(us_partition, aes(change_year)) + 
        geom_histogram(binwidth = 1, fill=cluster_colors[i]) + 
        xlim(c(1975, 2015)) +
        ylim(c(0, 27)) +
        labs(title=paste('Change Year distribution of', cluster_names[i]))
  
  filename <- paste('plots/cluster distribution/year_histogram_', i, '.png', sep='')
  ggsave(filename, g, width=12, height=6, units='in')
  plot(g)
}

# some summary stats
mean_lats <- tapply(valid_us$station_lat, valid_us$cluster, mean)
mean_lons <- tapply(valid_us$station_lon, valid_us$cluster, mean)
mean_change_year <- tapply(valid_us$change_year, valid_us$cluster, mean)
mean_change_amounts <- tapply(valid_us$change_amount, valid_us$cluster, mean)

print(paste(cluster_names, round(mean_change_year)))
print(paste(cluster_names, round(mean_change_amounts, 2)))

# put them into a df to plot
mean_df <- cbind(mean_lats, mean_lons, mean_change_year, mean_change_amounts)
mean_df <- cbind(cluster_names, data.frame(mean_df))

# visualize cluster centers
g <- ggplot() + geom_polygon(data = states, aes(x=long, y = lat, group = group), fill='lightgray', color="black") + 
  coord_fixed(1.3) + 
  geom_point(mean_df, mapping = aes(mean_lons, mean_lats, color=cluster_names), size=2) +
  geom_point(mapping=aes(mean(valid_us$station_lon), mean(valid_us$station_lat)), size=3, shape=17) +
  #scale_color_gradientn(name='Change  \n Year', colors=c('red', 'white', 'blue'), limits=c(1975, 2015)) +
  labs(title=paste('Spatial distribution of cluster means')) +
  theme_void(base_size=20)

plot(g)

#### Functional boxplots ##########################
# set graphical parameter to minimize margins
par(mar=c(4,4,2,1))

mat_list <- list()

# create fbplot for each cluster
for(i in 1:5){
  # get data from this cluster's partition
  us_partition <- valid_us[which(valid_us$cluster == i),]
  
  # set up a matrix to hold ts values for that cluster
  series_matrix <- matrix(nrow=0,ncol=31)
  
  # iterate through stations
  # will produce warnings due to how we use window() fxn
  for(station in us_partition$station_id){
    # re-build ts
    station_df <- valid_dma8[which(valid_dma8$station_id == station),]
    start_year <- as.numeric(substring(min(as.Date(station_df$datetime)), 0, 4))
    station_data <- ts(station_df$avgdma8epax, start=start_year)
    
    # if we have data from 1985-2010, add to matrix w/ all ts
    if(length(window(station_data, start=1980, end=2010)) == 31){
      series_matrix <- rbind(series_matrix, window(station_data, start=1980, end=2010))
    }
  }
  
  # want this info for future boxplots
  mat_list[[i]] <- series_matrix
  
  # create the functional boxplot
  fbplot(t(series_matrix), main=cluster_names[i], xlab='Year', ylab='8-hour Mixing Ratio', ylim=c(0,120), xaxt='n', color=cluster_colors[i])#, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
  axis(1, at=c(0,5,10,15,20,25,30), labels=c(1980,1985,1990,1995,2000,2005,2010))# ,cex.axis=1.5)
  title(ylab=)
  # testing: print the cluster means for each year
  # print(apply(series_matrix, 2, mean))
}

# undo parameter shenanigans
dev.off()

#### Other boxplots
# get all of our ts into one matrix with cluster info
for(i in 1:5){
  mat_list[[i]] <- cbind(mat_list[[i]], i)
}
