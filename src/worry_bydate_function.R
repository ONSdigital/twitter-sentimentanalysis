#function to create a mean worry scores for day, week, month
#e.g. to create mean for month; means_by_month<- worry_function("C:\\Users\\Alan Evans\\Documents\\output_r1.csv", 2014, "month")

worry_function <- function(input, year, frequency) {
  
  data <- read.csv(input, 
                   header = TRUE, 
                   stringsAsFactors = FALSE) %>%
    as_tibble()
  
  
  if (frequency == "date") {
    output <- data %>% 
      mutate(date = ymd(date)) %>% 
      group_by(date) %>% 
      summarise(mean_worry = mean(worry))
  }
  
  if (frequency == "week") {
    output <- data %>% 
      mutate(week = week(ymd(date))) %>% 
      group_by(week) %>% 
      summarise(mean_worry = mean(worry)) %>%
      mutate(date = dmy(paste0("01-01-", year)) + weeks(week))
  }
  
  if (frequency == "month") {
    output <-  data %>% 
      mutate(month = month(ymd(date))) %>% 
      group_by(month) %>% 
      summarise(mean_worry = mean(worry)) %>% 
      mutate(date = dmy(paste0("01-01-", year)) + months(month))
  }
  
  output                   
  
}
