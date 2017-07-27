#import geography file

geolookup <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/LSOA11 lookup.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)%>%
                  select(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM, RGN11CD, RGN11NM) %>%
                   rename (lsoa11=LSOA11CD)

#join geography to scored tweets
data_geo <- scored_data_afinn %>%
  inner_join(geolookup, by = "lsoa11")

#get rid of NA sentiment scores
data_geo <- data_geo %>%
  filter(afinn != "NA")

#calculate mean by region
 region_mean <- data_geo %>%
  group_by(RGN11NM) %>%
  summarise(mean=mean(afinn), number=n(), sum=sum(afinn))
 
 #calculate mean by LA
 la_mean <- data_geo %>%
   group_by(LAD11NM) %>%
   summarise(mean=mean(afinn), number=n(), sum=sum(afinn))
 
 #plot sentiment by region 
 ggplot(data=region_mean, aes(x=RGN11NM, y=mean)) + 
   geom_bar(stat="identity")
 
 #plot sentiment by LA
 ggplot(data=la_mean, aes(x=reorder(LAD11NM, mean), y=mean)) + 
   geom_bar(stat="identity")

 