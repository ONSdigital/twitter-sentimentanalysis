#Pull in test.csv file

scored_data <- read.csv("C:/Users/Alan Evans/Documents/test.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)


dates <- as.Date(scored_data$date) 
dates_formatted <-  month(as.POSIXlt(dates, format = "%Y/%M/%D"))
scored_data$month <- dates_formatted


dates_formatted <-  week(as.POSIXlt(dates, format = "%Y/%M/%D"))
scored_data$week <- dates_formatted

mean_week <- scored_data %>%
  group_by(week) %>%
  summarise(afinn_mean = mean(afinn),worry_mean = mean(worry)) 

mean_month <- scored_data %>%
  group_by(month) %>%
  summarise(afinn_mean = mean(afinn),worry_mean = mean(worry)) 



ggplot(mean_month, aes(x=month,y = afinn_mean)) +geom_bar(stat = "identity")
ggplot(mean_week, aes(x=week,y = worry_mean)) +geom_bar(stat = "identity")


week_date <-  week(as.POSIXlt(fu, format = "%Y/%M/%D"))

View(bar)
