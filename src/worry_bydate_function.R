#function to create a mean worry scores for day, week, month
#e.g. to create mean for month; means_by_month<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv", 2014, "month")

worry_function <- function(input, year, frequency, sampletag) {
  
  data <- read.csv(input, 
                   header = TRUE, 
                   stringsAsFactors = FALSE) %>%
    as_tibble()
  
  
  if (frequency == "date") {
    output <- data %>% 
      mutate(date = ymd(date)) %>% 
      group_by(date) %>% 
      summarise(mean_worry = mean(worry))%>%
      mutate(sample = sampletag)
  }
  
  if (frequency == "week") {
    output <- data %>% 
      mutate(week = week(ymd(date))) %>% 
      group_by(week) %>% 
      summarise(mean_worry = mean(worry)) %>%
      mutate(date = dmy(paste0("01-01-", year)) + weeks(week))%>%
      mutate(sample = sampletag)
  }
  
  if (frequency == "month") {
    output <-  data %>% 
      mutate(month = month(ymd(date))) %>% 
      group_by(month) %>% 
      summarise(mean_worry = mean(worry)) %>% 
      mutate(date = dmy(paste0("01-01-", year)) + months(month))%>%
      mutate(sample = sampletag)
  }
  
  output                   
  
}

#plots by time period 

plot_function_worry <- function(chart, frequency){
  ggplot(chart, aes(x= date,y = mean_worry)) +
    geom_smooth(se=FALSE, colour="paleturquoise3", size=1.3) +
    geom_line(colour="deepskyblue", size=1) +
    ggtitle (paste0("Mean worry Score by ", frequency)) +
    labs(x= paste0(frequency," of the year") ,y="Mean worry Score") +
    theme(axis.line = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}

#produce mean scores from output file
worry_means_week_r1<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv",
                                     2014, 
                                     "week", 1) 

worry_means_week_r2<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r2.csv",
                                     2014, 
                                     "week", 2) 

worry_means_week_r3<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r3.csv",
                                     2014, 
                                     "week", 3) 

worry_means_month_r4<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r4.csv",
                                      2014, 
                                      "week", 4)

worry_means_week_r5<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv",
                                     2014, 
                                     "week", 5) 
#Plots for day
plot_function_worry(worry_means_day_r1, "Date")
plot_function_worry(worry_means_day_r2, "Date")
plot_function_worry(worry_means_day_r3, "Date")
plot_function_worry(worry_means_day_r4, "Date")
plot_function_worry(worry_means_day_r5, "Date")

#plots for week
plot_function_worry(worry_means_week_r1, "week")
plot_function_worry(worry_means_week_r2, "week")
plot_function_worry(worry_means_week_r3, "week")
plot_function_worry(worry_means_week_r4, "week")
plot_function_worry(worry_means_week_r5, "week")

#plots by month
plot_function_worry(worry_means_month_r1, "month")
plot_function_worry(worry_means_month_r2, "month")
plot_function_worry(worry_means_month_r3, "month")
plot_function_worry(worry_means_month_r4, "month")
plot_function_worry(worry_means_month_r5, "month")