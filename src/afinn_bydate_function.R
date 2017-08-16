#function to create a mean afinn scores for date, week, month and outputs a csv
#values for frequency are "date", "week", "month"
#Can tag with a sample flag indicator for further analysis e.g. 1 or "Alpha"
#e.g. to create mean for month; means_by_month<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv", 2014, "date")

afinn_function <- function(input, year, frequency, sampletag) {
  
  data <- read.csv(input, 
                   header = TRUE, 
                   stringsAsFactors = FALSE) %>%
    as_tibble()
  
  
  if (frequency == "date") {
    output <- data %>% 
      mutate(date = ymd(date)) %>% 
      group_by(date) %>% 
      summarise(mean_afinn = mean(afinn), number =n())%>%
      mutate(sample = sampletag)
    
  }
  
  if (frequency == "week") {
    output <- data %>% 
      mutate(week = week(ymd(date))) %>% 
      group_by(week) %>% 
      summarise(mean_afinn = mean(afinn), number =n()) %>%
      mutate(date = dmy(paste0("01-01-", year)) + weeks(week))%>%
      mutate(sample = sampletag)
    
  }
  
  if (frequency == "month") {
    output <-  data %>% 
      mutate(month = month(ymd(date))) %>% 
      group_by(month) %>% 
      summarise(mean_afinn = mean(afinn), number =n()) %>% 
      mutate(date = dmy(paste0("01-01-", year)) + months(month))%>%
      mutate(sample = sampletag)
  }
  
  output             
}


#plots by time period 

plot_function_afinn <- function(chart, frequency){
  ggplot(chart, aes(x= date,y = mean_afinn)) +
    geom_smooth(se=FALSE, colour="paleturquoise3", size=1.3) +
    geom_line(colour="deepskyblue", size=1) +
    ggtitle (paste0("Mean afinn Score by ", frequency)) +
    labs(x= paste0(frequency," of the year") ,y="Mean afinn Score") +
    theme(axis.line = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}

#produce mean scores by date from output file
afinn_means_date_r1<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv",
                                    2014, 
                                    "date", 1 ) 

afinn_means_date_r2<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r2.csv",
                                    2014, 
                                    "date", 2) 

afinn_means_date_r3<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r3.csv",
                                    2014, 
                                    "date", 3) 

afinn_means_date_r4<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r4.csv",
                                    2014, 
                                    "date", 4)

afinn_means_date_r5<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv",
                                    2014, 
                                    "date", 5) 

#produce mean scores by week from output file
afinn_means_week_r1<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv",
                                    2014, 
                                    "week", 1 ) 

afinn_means_week_r2<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r2.csv",
                                    2014, 
                                    "week", 2) 

afinn_means_week_r3<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r3.csv",
                                    2014, 
                                    "week", 3) 

afinn_means_week_r4<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r4.csv",
                                    2014, 
                                    "week", 4)

afinn_means_week_r5<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv",
                                    2014, 
                                    "week", 5) 

#produce mean scores by month from output file
afinn_means_month_r1<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv",
                                    2014, 
                                    "month", 1 ) 

afinn_means_month_r2<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r2.csv",
                                    2014, 
                                    "month", 2) 

afinn_means_month_r3<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r3.csv",
                                    2014, 
                                    "month", 3) 

afinn_means_month_r4<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r4.csv",
                                    2014, 
                                    "month", 4)

afinn_means_month_r5<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv",
                                    2014, 
                                    "month", 5) 


#Plots for day
plot_function_afinn(afinn_means_date_r1, "Date")
plot_function_afinn(afinn_means_date_r2, "Date")
plot_function_afinn(afinn_means_date_r3, "Date")
plot_function_afinn(afinn_means_date_r4, "Date")
plot_function_afinn(afinn_means_date_r5, "Date")

#plots for week
plot_function_afinn(afinn_means_week_r1, "week")
plot_function_afinn(afinn_means_week_r2, "week")
plot_function_afinn(afinn_means_week_r3, "week")
plot_function_afinn(afinn_means_week_r4, "week")
plot_function_afinn(afinn_means_week_r5, "week")

#plots by month
plot_function_afinn(afinn_means_month_r1, "month")
plot_function_afinn(afinn_means_month_r2, "month")
plot_function_afinn(afinn_means_month_r3, "month")
plot_function_afinn(afinn_means_month_r4, "month")
plot_function_afinn(afinn_means_month_r5, "month")

#plot for all 5 samples of worry by date

ggplot(afinn_means_date_r1, aes(x= date,y = mean_afinn, colour= sample)) +
  geom_line() +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_date_r2) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_date_r3) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_date_r4) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_date_r5) +
  ggtitle ("Mean afinn score by date")+
  labs (x= "Month of the year" ,y="Mean afinn score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#plot for all 5 samples of worry by week

ggplot(afinn_means_week_r1, aes(x= date,y = mean_afinn, colour= sample)) +
  geom_line() +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_week_r2) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_week_r3) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_week_r4) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_week_r5) +
  ggtitle ("Mean afinn score by week")+
  labs (x= "Month of the year" ,y="Mean afinn score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

#plot for all 5 samples of worry by month

ggplot(afinn_means_month_r1, aes(x= date,y = mean_afinn, colour= sample)) +
  geom_line() +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_month_r2) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_month_r3) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_month_r4) +
  geom_line(aes(x= date,y = mean_afinn), afinn_means_month_r5) +
  ggtitle ("Mean afinn score by month")+
  labs (x= "Month of the year" ,y="Mean afinn score") +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
