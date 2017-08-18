#importing a shape file, converting one of our outputted scored files into a format to create a region map
#joining shape file with data file
#plotting shape file by number and then by mean score

library(rgdal)
library(maptools)
library(data.table)
library(RColorBrewer)
library(classInt)
library(rtweet)
library(tidyverse)
library(tidytext)
 

#Read in your data files and then combine them into one large data set



geo_scored_data <- data %>%
  inner_join(geolookup, by = "lsoa11")

final_data1_mean <- geo_scored_data %>%
  group_by(RGN11NM) %>%
  summarise (mean_worry= mean(worry),number=n())%>%
  filter(RGN11NM != "Wales")

#Read in shape file
region_shapefile <- readOGR("C:/Users/Alan Evans/Documents/region Shape file", "Regions_December_2015_Full_Extent_Boundaries_in_England")

#change column name in shaoe file to match column name in df
region_shapefile@data <- setnames(region_shapefile@data, "rgn15nm", "RGN11NM")

#join the data sets together
region_shapefile@data <-  region_shapefile@data %>%
  
  left_join(final_data1_mean, by='RGN11NM')


# Create map 1 - purple - Number of tweets

  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(region_shapefile@data$number.x,
                        n=7, # set the number of ranges to create
                        style="fixed",  fixedBreaks= c(5000, 8000, 11000, 14000, 17000, 20000)) # set the algorithm to use to create the ranges
  
  #get 4 Purple ColorBrewer Colours
  ColourSchemePurple <- brewer.pal(7,"Purples")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(region_shapefile,
       col= ColourSchemePurple[findInterval(region_shapefile@data$number.x, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  plot(region_subset(region_shapefile, "RGN11M"),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  

  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset = c(-0.07),
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemePurple), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Number of tweets (n)")
  
  # Create map 2 - green worry mean
  
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(region_shapefile@data$mean_worry.x, 
                        n=5, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 blue/green ColorBrewer Colours
  ColourSchemeBuGn <- brewer.pal(5,"BuGn")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(region_shapefile,
       col= ColourSchemeBuGn[findInterval(region_shapefile@data$mean_worry.x, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset = c(-0.07),
         legend = leglabs(round(breaks$brks,3), reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemeBuGn), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Mean score of worry")
         

