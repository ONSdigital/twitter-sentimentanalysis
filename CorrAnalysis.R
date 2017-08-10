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

dates_day_formatted <-  date(as.POSIXlt(dates, format = "%Y/%M/%D"))
scored_data$date <- dates_day_formatted

mean_time_period <- scored_data %>%
  group_by(...) %>%
  summarise(afinn_mean = mean(afinn),worry_mean = mean(worry)) 
}

means_by_day_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\output_5.csv", date)

means_by_date_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\output_5.csv", week)

means_by_month_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\output_5.csv", month)


## Correlation analysis
# find correlation coefficients between the average sentiment scores and plot the relationships

library(tidyverse)

# Read in the data
ftse100 = read_tsv("FTSE100.txt")
sun = read_tsv("Sunshine.txt")
#meantemp = read_tsv("Mean temperature.txt")
#rain = read_tsv("Rainfall.txt")

## Convert dates into the same formate as mean_by_...
# Convert dates in ftse100 into the same format at in means_by_day
dateftse <- as.Date(ftse100$DATE, "%m/%d/%Y") 
dateftse_formatted <-  date(as.POSIXlt(dateftse, format = "%Y/%m/%d"))
ftse100$date <- dateftse_formatted
# Convert dates in sun into the same format at in means_by_month
dates <- as.Date(sun$month, "%b") 

# Join the ftse100 data to the means_by_day data
means_by_day_1 = inner_join(means_by_day_1,ftse100)
means_by_day_2 = inner_join(means_by_day_2,ftse100)
means_by_day_3 = inner_join(means_by_day_3,ftse100)
means_by_day_4 = inner_join(means_by_day_4,ftse100)
means_by_day_5 = inner_join(means_by_day_5,ftse100)
# Join the sun data to the means_by_month data
sun$Month = sun$month
means_by_month_1$Month = means_by_month_1$month
means_by_month_2$Month = means_by_month_2$month
means_by_month_3$Month = means_by_month_3$month
means_by_month_4$Month = means_by_month_4$month
means_by_month_5$Month = means_by_month_5$month
means_by_month_1$Month<-recode(means_by_month_1$Month,"4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                               10='Oct';11='Nov';12='Dec'")
means_by_month_2$Month<-recode(means_by_month_1$Month,"4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                               10='Oct';11='Nov';12='Dec'")
means_by_month_3$Month<-recode(means_by_month_1$Month,"4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                               10='Oct';11='Nov';12='Dec'")
means_by_month_4$Month<-recode(means_by_month_1$Month,"4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                               10='Oct';11='Nov';12='Dec'")
means_by_month_5$Month<-recode(means_by_month_1$Month,"4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                               10='Oct';11='Nov';12='Dec'")

sunmonth1 = merge(sun, means_by_month_1, by="Month")
sunmonth1 <- subset(sunmonth1, select = -c(month.x,month.y) )



## Find correlations of the extra variables with worry and afinn
# Correlation of sunshine with worry and afinn
corsunafinn = cor(sunmonth$`Total hours`,sunmonth$afinn_mean)
corsunworry = cor(sunmonth$`Total hours`,sunmonth$worry_mean)
# Create a dataframe with names to store the correlations
corrnames = c(a="ftse afinn", b="ftse worry", c="sun afinn", d="sun worry", e="temp afinn", f="temp worry",
              g="rain afinn", h="rain worry")
names(corrnames)

## Create plots of the extra variables with afinn and worry for each of the 5 samples of the main dataset
# Call the package for plotting graphs with 2 y axes
library(plotrix)
# Plot 2 y axis graphs for ftse on all 5 samples
twoord.plot(means_by_day_1$date, means_by_day_1$afinn_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`)
twoord.plot(means_by_day_1$date, means_by_day_1$worry_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`)
twoord.plot(means_by_day_2$date, means_by_day_2$afinn_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`)
twoord.plot(means_by_day_2$date, means_by_day_2$worry_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`)
twoord.plot(means_by_day_3$date, means_by_day_3$afinn_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`)
twoord.plot(means_by_day_3$date, means_by_day_3$worry_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`)
# Plot 2 y axis graphs for sun on all 5 samples
twoord.plot(means_by_day_1$date, means_by_day_1$afinn_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`)
twoord.plot(means_by_day_1$date, means_by_day_1$worry_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`)
twoord.plot(means_by_day_2$date, means_by_day_2$afinn_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`)
twoord.plot(means_by_day_2$date, means_by_day_2$worry_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`)
twoord.plot(means_by_day_3$date, means_by_day_3$afinn_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`)
twoord.plot(means_by_day_3$date, means_by_day_3$worry_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`)
# Plot 2 y axis graphs for temp on all 5 samples
# Plot 2 y axis graphs for rain on all 5 samples