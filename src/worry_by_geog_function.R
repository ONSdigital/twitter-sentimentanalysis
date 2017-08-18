#install.packages("RCurl")
library(RCurl)
library(tidyverse)


#import geography file

geog_function<-function(input, granularity){

geolookup <- read.csv(text=getURL("https://raw.githubusercontent.com/ONSdigital/twitter-sentimentanalysis/master/data/LSOA11%20lookup.csv"),
                 header = TRUE, 
                 stringsAsFactors = FALSE)%>%
                  select(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM, RGN11CD, RGN11NM) %>%
                   rename (lsoa11=LSOA11CD)

#import scored data

scored_data <- read.csv(input,
                  header = TRUE, 
                  stringsAsFactors = FALSE)%>%
  as.tibble()

#join geography to scored tweets

geo_scored_data <- scored_data %>%
  inner_join(geolookup, by = "lsoa11")

#calculate mean afinn and worry by region

if (granularity == "RGN11NM"){
  output <- geo_scored_data %>%
  group_by(RGN11NM) %>%
  summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
}
 
if (granularity == "LAD11NM"){
  output <- geo_scored_data %>%
    group_by(LAD11NM) %>%
    summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
}

if (granularity == "LSOA11NM"){
  output <- geo_scored_data %>%
    group_by(LSOA11NM) %>%
    summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
}
output
}


#plot worry  (highest 25 LAs)
la_plot1 <- la_mean_1 %>% filter(mean_worry<0.3175) %>%
  ggplot(aes(x=reorder(LAD11NM,mean_worry), y=mean_worry)) + 
  geom_bar(stat="identity", fill='paleturquoise3') +
  ggtitle ("The 25 local authorities with the highest mean worry score")+
  labs (x= "Local authority" ,y="Mean worry score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(la_plot1)

#plot worry by LA (lowest 25 LAs)
la_plot2 <- la_mean_1 %>% filter(mean_worry<0.3175) %>%
  ggplot(aes(x=reorder(LAD11NM,mean_worry), y=mean_worry)) + 
  geom_bar(stat="identity", fill='paleturquoise3') +
  ggtitle ("The 25 local authorities with the highest mean worry score")+
  labs (x= "Local authority" ,y="Mean worry score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(la_plot2)

#plot sentiment by region 
ggplot(region_mean_1,aes(x=reorder(RGN11NM,mean_worry), y=mean_worry)) + 
  geom_bar(stat="identity", fill='paleturquoise3') +
  ggtitle ("Mean worry score by region")+
  labs (x= "Region" ,y="Mean worry score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

#plot number of tweets by LA 
ggplot(la_mean_1,aes(x=reorder(LAD11NM,number), y=number)) + 
  geom_bar(stat="identity", fill='paleturquoise3') +
  ggtitle ("Number of tweets by local authority")+
  labs (x= "Local authority" ,y="Number of tweets in sample") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

#plot number of tweets by region 
ggplot(region_mean_1,aes(x=reorder(RGN11NM,number), y=number)) + 
  geom_bar(stat="identity", fill='paleturquoise3') +
  ggtitle ("Number of tweets by region")+
  labs (x= "Region" ,y="Number of tweets in sample") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))




 
 

 