library(lubridate)
library(dplyr)

#Pull in test.csv file
analysis_by_date<- function(input, ...){
 scored_data <- read.csv(input, 
                         header = TRUE, 
                         stringsAsFactors = FALSE)
 
dates <- as.Date(scored_data$date) 
dates_formatted <-  month(as.POSIXlt(dates, format = "%Y/%M/%D"))
scored_data$month <- dates_formatted

dates_week_formatted <-  week(as.POSIXlt(dates, format = "%Y/%M/%D"))
scored_data$week <- dates_week_formatted

mean_time_period <- scored_data %>%
  group_by(...) %>%
  summarise(afinn_mean = mean(afinn),worry_mean = mean(worry)) 
}

means_by_date<- analysis_by_date("C:\\Users\\Joseph Jenkins\\Documents\\test.csv", week)

means_by_month<- analysis_by_date("C:\\Users\\Joseph Jenkins\\Documents\\test.csv", month)

