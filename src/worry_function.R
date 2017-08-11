#function to create a mean worry scores for day, week, month and outputs a csv
#e.g. to create mean for month; means_by_month<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv", 2014, "week", "C:\\Users\\Alan Evans\\Desktop\\bar.csv")

worry_function <- function(input, year, frequency, output) {
  
  data <- read.csv(input, 
                   header = TRUE, 
                   stringsAsFactors = FALSE) %>%
    as_tibble()
  
  
  if (frequency == "date") {
    output <- data %>% 
      mutate(date = ymd(date)) %>% 
      group_by(date) %>% 
      summarise(mean_worry = mean(worry))%>%
      write.csv(output)
  }
  
  if (frequency == "week") {
    output <- data %>% 
      mutate(week = week(ymd(date))) %>% 
      group_by(week) %>% 
      summarise(mean_worry = mean(worry)) %>%
      mutate(date = dmy(paste0("01-01-", year)) + weeks(week))%>%
      write.csv(output) 
  }
  
  if (frequency == "month") {
    output <-  data %>% 
      mutate(month = month(ymd(date))) %>% 
      group_by(month) %>% 
      summarise(mean_worry = mean(worry)) %>% 
      mutate(date = dmy(paste0("01-01-", year)) + months(month))%>%
      write.csv(output) 
  }
  
                   
  
}