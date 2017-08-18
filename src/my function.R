#plots by time period 

plot_function_afinn <- function(chart, frequency){
  ggplot(chart, aes(x= date,y = mean_afinn)) +
    geom_smooth(se=FALSE, colour="paleturquoise3", size=1.3) +
    geom_point(colour="deepskyblue", size=2) +
    ggtitle (paste0("Mean Worry Score by ", frequency)) +
    labs(x= paste0(frequency," of the year") ,y="Mean Worry Score") +
    theme(axis.line = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}

x<- plot_function_afinn(afinn_means_month_r1, "Month")

data5<- read.csv("C://Users//Alan Evans//Documents/output_r5.csv", stringsAsFactors = FALSE, header = TRUE)


#creating map file
library(rgdal)
install.packages("maptools")
library(maptools)
library(data.table)
install.packages("RColorBrewer")
library(RColorBrewer)

region_shapefile <- readOGR("C:\\Users\\Alan Evans\\Documents\\region Shape file" ,Regions_December_2015_Full_Extent_Boundaries_in_England.shp)
plot (region_shapefile)

region_shapefile@data <- setnames(region_shapefile@data, "rgn15nm", "RGN11NM")

region_shapefile@data <-  region_shapefile@data %>%
  
  left_join(region_mean, by='RGN11NM')

join_region_data_to_shapefile <- function(your_dataset, shapefile_name){

  region_shapefile <- merge(region_shapefile, region_mean)
  
  region_shapefile@data <-  region_shapefile@data %>%
    
    left_join(region_mean, by='RGN11NM')
  
  return(region_mean$mean_worry)
  
}
ColourSchemeRed <- brewer.pal(4,"Reds")
plot (region_shapefile)
join_region_data_to_shapefile(region_mean, region_shapefile)

plot(region_shapefile, region_shapefile@data, region_mean$mean_worry)


summary(region_shapefile)
plot(region_shapefile, 
      col = region_shapefile@data$number)
head(region_shapefile)




View(region_shapefile)


LSOA11 <- data %>% 
    group_by(lsoa11) %>% 
    summarise(mean_worry = mean(worry)) %>%
    mutate(date = dmy(paste0("01-01-", year)) + weeks(week))
  

ggplot(worry_means_month_r1, aes(x= date,y = mean_worry, colour = sample)) +
  geom_line() +
  geom_line(aes(x= date,y = mean_worry), worry_means_month_r2) +
  geom_line(aes(x= date,y = mean_worry), worry_means_month_r3) +
  geom_line(aes(x= date,y = mean_worry), worry_means_month_r4) +
  geom_line(aes(x= date,y = mean_worry), worry_means_month_r5) +
  scale_fill_brewer(palette = "Set1") + 
  ggtitle ("Mean worry score by month")+
  labs (x= "Month of the year" ,y="Mean worry score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))


output <- data %>% 
  mutate(date = ymd(date)) %>% 
  group_by(date) %>% 
  summarise(mean_worry = mean(worry), number =n())



ggplot(data=worry_means_date_r3, aes(x=date,  y=number)) +
  geom_line(stat="identity", fill='paleturquoise3') +
  ggtitle ("Number of tweets by region")+
  labs (x= "Region" ,y="Number of tweets in sample") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(worry_means_date_r1, aes(x= date,y = number, colour= sample)) +
  geom_line() +
  geom_line(aes(x= date,y = number), worry_means_date_r2) +
  geom_line(aes(x= date,y = number), worry_means_date_r3) +
  geom_line(aes(x= date,y = number), worry_means_date_r4) +
  geom_line(aes(x= date,y = number), worry_means_date_r5) +
  ggtitle ("Number of tweets by month")+
  labs (x= "Month of the year" ,y="Number of tweets") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggplot(output, aes(x= dow,y = number, group =1, colour = sample_flag)) +
  geom_line() +
  geom_line(aes(x= dow,y = number), output2) +
  geom_line(aes(x= dow,y = number), output3) +
  geom_line(aes(x= dow,y = number), output4) +
  geom_line(aes(x= dow,y = number), output5) +
  ggtitle ("Number of tweets by month")+
  labs (x= "Month of the year" ,y="Number of tweets") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))


output <- data %>% 
  mutate(date = ymd(date)) %>% 
  group_by(dow) %>% 
  filter(month != "Nov")%>%
  summarise(mean_worry = mean(worry),number =n())%>%
  mutate(sample_flag = c(1))



