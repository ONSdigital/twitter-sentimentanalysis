library(lubridate)
library(dplyr)

#Pull in test.csv file
analysis_by_date<- function(input, year, ...){
 scored_data <- read.csv(input, 
                         header = TRUE, 
                         stringsAsFactors = FALSE)
 
dates <- as.Date(scored_data$date) 
dates_formatted <-  month(ymd(dates))
scored_data$month <- dates_formatted


dates_week_formatted <-  week(ymd(dates))
scored_data$week <- dates_week_formatted

mean_time_period <- scored_data %>%
  group_by(...) %>%
  summarise(afinn_mean = mean(afinn),worry_mean = mean(worry))%>%
  mutate(week_of_year = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u"))
  
}

means_by_date2<- analysis_by_date("C:\\Users\\Alan Evans\\Documents\\output_r1.csv",2014, week)

means_by_month<- analysis_by_date("C:\\Users\\Joseph Jenkins\\Documents\\output_r1.csv", month)


