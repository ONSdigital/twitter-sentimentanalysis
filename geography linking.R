#import geography file

geolookup <- read.csv("C:/Users/Alan Evans/Documents/LSOA11 lookup.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)%>%
                  select(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM, RGN11CD, RGN11NM) %>%
                   rename (lsoa11=LSOA11CD)

#import scored data

scored_data1 <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_scored.csv",
                  header = TRUE, 
                  stringsAsFactors = FALSE)

scored_data2 <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_r2_scored.csv",
                         header = TRUE, 
                         stringsAsFactors = FALSE)

scored_data3 <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_r3_scored.csv",
                         header = TRUE, 
                         stringsAsFactors = FALSE)

scored_data4 <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_r4_scored.csv",
                         header = TRUE, 
                         stringsAsFactors = FALSE)

scored_data5 <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_r5_scored.csv",
                         header = TRUE, 
                         stringsAsFactors = FALSE)

#join geography to scored tweets

geo_scored_data1 <- scored_data1 %>%
  inner_join(geolookup, by = "lsoa11")

geo_scored_data2 <- scored_data2 %>%
  inner_join(geolookup, by = "lsoa11")

geo_scored_data3 <- scored_data3 %>%
  inner_join(geolookup, by = "lsoa11")

geo_scored_data4 <- scored_data4 %>%
  inner_join(geolookup, by = "lsoa11")

geo_scored_data5 <- scored_data5 %>%
  inner_join(geolookup, by = "lsoa11")

#calculate mean afinn and worry by region

 region_mean_1 <- geo_scored_data1 %>%
  group_by(RGN11NM) %>%
  summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
 
 region_mean_2 <- geo_scored_data2 %>%
   group_by(RGN11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
 
 region_mean_3 <- geo_scored_data3 %>%
   group_by(RGN11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
 
 region_mean_4 <- geo_scored_data4 %>%
   group_by(RGN11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n())
 
 region_mean_5 <- geo_scored_data5 %>%
   group_by(RGN11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry),number=n()
 
 #calculate mean by LA
 la_mean_1 <-geo_scored_data1 %>%
   group_by(LAD11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry), number=n())

 la_mean_2 <-geo_scored_data2 %>%
   group_by(LAD11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry), number=n())
 
 la_mean_3 <-geo_scored_data3 %>%
   group_by(LAD11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry), number=n())
 
 la_mean_4 <-geo_scored_data4 %>%
   group_by(LAD11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry), number=n())
 
 la_mean_5 <-geo_scored_data5 %>%
   group_by(LAD11NM) %>%
   summarise(mean_afinn=mean(afinn), mean_worry= mean(worry), number=n())
 
 #plot sentiment by region 
 ggplot(data=region_mean_1, aes(x=RGN11NM, y=mean_afinn)) + 
   geom_bar(stat="identity")
 
 #plot sentiment by LA
 ggplot(data=la_mean_1, aes(x=reorder(LAD11NM, mean), y=mean_afinn)) + 
   geom_bar(stat="identity")
 
 

 