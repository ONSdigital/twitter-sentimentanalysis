#function to create a mean afinn scores for day, week, month and outputs a csv
#e.g. to create mean for month; means_by_month<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv", 2014, "date", "C:\\Users\\Alan Evans\\Desktop\\foo.csv")

afinn_function <- function(input, year, frequency, output) {
  
  data <- read.csv(input, 
                   header = TRUE, 
                   stringsAsFactors = FALSE) %>%
    as_tibble()
                   
                
                   if (frequency == "date") {
                     output <- data %>% 
                       mutate(date = ymd(date)) %>% 
                       group_by(date) %>% 
                       summarise(mean_afinn = mean(afinn))%>%
                       write.csv(output)
                   }
                   
                   if (frequency == "week") {
                     output <- data %>% 
                       mutate(week = week(ymd(date))) %>% 
                       group_by(week) %>% 
                       summarise(mean_afinn = mean(afinn)) %>%
                       mutate(date = dmy(paste0("01-01-", year)) + weeks(week))%>%
                       write.csv(output)
                   }
                   
                   if (frequency == "month") {
                    output <-  data %>% 
                       mutate(month = month(ymd(date))) %>% 
                       group_by(month) %>% 
                       summarise(mean_afinn = mean(afinn)) %>% 
                       mutate(date = dmy(paste0("01-01-", year)) + months(month))%>%
                      write.csv(output)
                   }
                   
                   
}


afinn_means_week_r5<- afinn_function("C:\\Users\\Alan Evans\\Documents\\output_r5.csv", 2014, "date", "C:\\Users\\Alan Evans\\Desktop\\foo.csv")

