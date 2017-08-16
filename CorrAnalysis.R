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

means_by_day_1 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output1.csv", date)
means_by_day_2 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output2.csv", date)
means_by_day_3 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output3.csv", date)
means_by_day_4 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output4.csv", date)
means_by_day_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output5.csv", date)

means_by_date_1 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output1.csv", week)
means_by_date_2 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output2.csv", week)
means_by_date_3 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output3.csv", week)
means_by_date_4 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output4.csv", week)
means_by_date_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output5.csv", week)

means_by_month_1 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output1.csv", month)
means_by_month_2 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output2.csv", month)
means_by_month_3 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output3.csv", month)
means_by_month_4 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output4.csv", month)
means_by_month_5 <- analysis_by_date("C:\\Users\\Michael Kearns\\Documents\\cleaned_outputs\\processed_output5.csv", month)


## Correlation analysis
# find correlation coefficients between the average sentiment scores and plot the relationships

library(tidyverse)
library(car)

# Read in the data
ftse100 = read_tsv("C:/Users/Michael Kearns/twitter-sentimentanalysis/Data/FTSE100.txt")
sun = read_tsv("C:/Users/Michael Kearns/twitter-sentimentanalysis/Data/Sunshine.txt")
#meantemp = read_tsv("C:/Users/Michael Kearns/twitter-sentimentanalysis/Data/Mean temperature.txt")
#rain = read_tsv("C:/Users/Michael Kearns/twitter-sentimentanalysis/Data/Rainfall.txt")


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

# Merge the sun light hours data with the afinn and worry scores. Also reorder the dataframe based on the number of the month
sunmonth1 = merge(sun, means_by_month_1, by="Month")
#sunmonth1 = subset(sunmonth1, select = -c(month.x,month.y) )
sunmonth1 = sunmonth1[order(sunmonth1$month.y),] 
sunmonth2 = merge(sun, means_by_month_2, by="Month")
#sunmonth2 = subset(sunmonth2, select = -c(month.x,month.y) )
sunmonth2 = sunmonth2[order(sunmonth2$month.y),] 
sunmonth3 = merge(sun, means_by_month_3, by="Month")
#sunmonth3 = subset(sunmonth3, select = -c(month.x,month.y) )
sunmonth3 = sunmonth3[order(sunmonth3$month.y),] 
sunmonth4 = merge(sun, means_by_month_4, by="Month")
#sunmonth4 = subset(sunmonth4, select = -c(month.x,month.y) )
sunmonth4 = sunmonth4[order(sunmonth4$month.y),] 
sunmonth5 = merge(sun, means_by_month_5, by="Month")
#sunmonth5 = subset(sunmonth5, select = -c(month.x,month.y) )
sunmonth5 = sunmonth5[order(sunmonth5$month.y),] 



## Find correlations of the extra variables with worry and afinn
# Correlation of the stock market with worry and afinn
corftseafinn1 = cor(means_by_day_1$`CLOSING PRICE`,means_by_day_1$afinn_mean)
corftseworry1 = cor(means_by_day_1$`CLOSING PRICE`,means_by_day_1$worry_mean)
corftseafinn2 = cor(means_by_day_2$`CLOSING PRICE`,means_by_day_2$afinn_mean)
corftseworry2 = cor(means_by_day_2$`CLOSING PRICE`,means_by_day_2$worry_mean)
corftseafinn3 = cor(means_by_day_3$`CLOSING PRICE`,means_by_day_3$afinn_mean)
corftseworry3 = cor(means_by_day_3$`CLOSING PRICE`,means_by_day_3$worry_mean)
corftseafinn4 = cor(means_by_day_4$`CLOSING PRICE`,means_by_day_4$afinn_mean)
corftseworry4 = cor(means_by_day_4$`CLOSING PRICE`,means_by_day_4$worry_mean)
corftseafinn5 = cor(means_by_day_5$`CLOSING PRICE`,means_by_day_5$afinn_mean)
corftseworry5 = cor(means_by_day_5$`CLOSING PRICE`,means_by_day_5$worry_mean)
# Correlation of sunshine with worry and afinn
corsunafinn1 = cor(sunmonth1$`Total hours`,sunmonth1$afinn_mean)
corsunworry1 = cor(sunmonth1$`Total hours`,sunmonth1$worry_mean)
corsunafinn2 = cor(sunmonth2$`Total hours`,sunmonth2$afinn_mean)
corsunworry2 = cor(sunmonth2$`Total hours`,sunmonth2$worry_mean)
corsunafinn3 = cor(sunmonth3$`Total hours`,sunmonth3$afinn_mean)
corsunworry3 = cor(sunmonth3$`Total hours`,sunmonth3$worry_mean)
corsunafinn4 = cor(sunmonth4$`Total hours`,sunmonth4$afinn_mean)
corsunworry4 = cor(sunmonth4$`Total hours`,sunmonth4$worry_mean)
corsunafinn5 = cor(sunmonth5$`Total hours`,sunmonth5$afinn_mean)
corsunworry5 = cor(sunmonth5$`Total hours`,sunmonth5$worry_mean)
# Create a dataframe with names to store the correlations
library(base)
corrnames = c(a="ftse", b="sun")
#, c="temp", d="rain")
varnames = c(a="afinn", b="worry") 
cors1 = c()
cors1$afinn = c(corftseafinn1,corsunafinn1)
cors1$worry = c(corftseworry1,corsunworry1)
cors1 = data.frame(matrix(unlist(cors1), nrow=2, byrow=F))
rownames(cors1) = corrnames
colnames(cors1) = varnames
cors2 = c()
cors2$afinn = c(corftseafinn2,corsunafinn2)
cors2$worry = c(corftseworry2,corsunworry2)
cors2 = data.frame(matrix(unlist(cors2), nrow=2, byrow=F))
rownames(cors2) = corrnames
colnames(cors2) = varnames
cors3 = c()
cors3$afinn = c(corftseafinn3,corsunafinn3)
cors3$worry = c(corftseworry3,corsunworry3)
cors3 = data.frame(matrix(unlist(cors3), nrow=2, byrow=F))
rownames(cors3) = corrnames
colnames(cors3) = varnames
cors4 = c()
cors4$afinn = c(corftseafinn4,corsunafinn4)
cors4$worry = c(corftseworry4,corsunworry4)
cors4 = data.frame(matrix(unlist(cors4), nrow=2, byrow=F))
rownames(cors4) = corrnames
colnames(cors4) = varnames
cors5 = c()
cors5$afinn = c(corftseafinn5,corsunafinn5)
cors5$worry = c(corftseworry5,corsunworry5)
cors5 = data.frame(matrix(unlist(cors5), nrow=2, byrow=F))
rownames(cors5) = corrnames
colnames(cors5) = varnames
write.table(cors1, "C:\\Users\\Michael Kearns\\Documents\\MC project 4\\Correlation Output\\cors1.csv", sep="\t")
write.table(cors2, "C:\\Users\\Michael Kearns\\Documents\\MC project 4\\Correlation Output\\cors2.csv", sep="\t")
write.table(cors3, "C:\\Users\\Michael Kearns\\Documents\\MC project 4\\Correlation Output\\cors3.csv", sep="\t")
write.table(cors4, "C:\\Users\\Michael Kearns\\Documents\\MC project 4\\Correlation Output\\cors4.csv", sep="\t")
write.table(cors5, "C:\\Users\\Michael Kearns\\Documents\\MC project 4\\Correlation Output\\cors5.csv", sep="\t")
#means_by_day_1$dte = means_by_day_1$DATE
#means_by_day_1$dte = as.Date(means_by_day_1$dte, "%m/%d/%Y")
#means_by_day_1$dte = as.numeric(means_by_day_1$dte)
#means_by_day_1$date = as.Date(means_by_day_1$date)
#means_by_day_1$DATE = as.Date(means_by_day_1$DATE)


## Create plots of the extra variables with afinn and worry for each of the 5 samples of the main dataset
# Call the package for plotting graphs with 2 y axes
library(plotrix)
# Plot 2 y axis graphs for ftse on all 5 samples
#means_by_day_1$Month = rep("Nov",145)
twoord.plot(means_by_day_1$date, means_by_day_1$afinn_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`,
            xlab = "Date",ylab = "Afinn mean",rylab = "FTSE100",xticklab = means_by_day_1$DATE)
twoord.plot(means_by_day_1$date, means_by_day_1$worry_mean,means_by_day_1$date,means_by_day_1$`CLOSING PRICE`,
            xlab = "Date",ylab = "Worry mean",rylab = "FTSE100",xticklab = means_by_day_1$date)
twoord.plot(means_by_day_2$date, means_by_day_2$afinn_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`,
            xlab = "Date",ylab = "Afinn mean",rylab = "FTSE100",xticklab = means_by_day_2$date)
twoord.plot(means_by_day_2$date, means_by_day_2$worry_mean,means_by_day_2$date,means_by_day_2$`CLOSING PRICE`,
            xlab = "Date",ylab = "Worry mean",rylab = "FTSE100",xticklab = means_by_day_2$date)
twoord.plot(means_by_day_3$date, means_by_day_3$afinn_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`,
            xlab = "Date",ylab = "Afinn mean",rylab = "FTSE100",xticklab = means_by_day_3$date)
twoord.plot(means_by_day_3$date, means_by_day_3$worry_mean,means_by_day_3$date,means_by_day_3$`CLOSING PRICE`,
            xlab = "Date",ylab = "Worry mean",rylab = "FTSE100",xticklab = means_by_day_3$date)
twoord.plot(means_by_day_4$date, means_by_day_4$afinn_mean,means_by_day_4$date,means_by_day_4$`CLOSING PRICE`,
            xlab = "Date",ylab = "Afinn mean",rylab = "FTSE100",xticklab = means_by_day_4$date)
twoord.plot(means_by_day_4$date, means_by_day_4$worry_mean,means_by_day_4$date,means_by_day_4$`CLOSING PRICE`,
            xlab = "Date",ylab = "Worry mean",rylab = "FTSE100",xticklab = means_by_day_4$date)
twoord.plot(means_by_day_5$date, means_by_day_5$afinn_mean,means_by_day_5$date,means_by_day_5$`CLOSING PRICE`,
            xlab = "Date",ylab = "Afinn mean",rylab = "FTSE100",xticklab = means_by_day_5$date)
twoord.plot(means_by_day_5$date, means_by_day_5$worry_mean,means_by_day_5$date,means_by_day_5$`CLOSING PRICE`,
            xlab = "Date",ylab = "Worry mean",rylab = "FTSE100",xticklab = means_by_day_5$date)
# Plot 2 y axis graphs for sun on all 5 samples
sunxlab = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
twoord.plot(sunmonth1$month.y, sunmonth1$afinn_mean,sunmonth1$month.y,sunmonth1$'Total hours',
            xlab = "Date",ylab = "Afinn mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth1$month.y, sunmonth1$worry_mean,sunmonth1$month.y,sunmonth1$`Total hours`,
            xlab = "Date",ylab = "Worry mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth2$month.y, sunmonth2$afinn_mean,sunmonth2$month.y,sunmonth2$'Total hours',
            xlab = "Date",ylab = "Afinn mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth2$month.y, sunmonth2$worry_mean,sunmonth2$month.y,sunmonth2$`Total hours`,
            xlab = "Date",ylab = "Worry mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth3$month.y, sunmonth3$afinn_mean,sunmonth3$month.y,sunmonth3$'Total hours',
            xlab = "Date",ylab = "Afinn mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth3$month.y, sunmonth3$worry_mean,sunmonth3$month.y,sunmonth3$`Total hours`,
            xlab = "Date",ylab = "Worry mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth4$month.y, sunmonth4$afinn_mean,sunmonth4$month.y,sunmonth4$'Total hours',
            xlab = "Date",ylab = "Afinn mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth4$month.y, sunmonth4$worry_mean,sunmonth4$month.y,sunmonth4$`Total hours`,
            xlab = "Date",ylab = "Worry mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth5$month.y, sunmonth5$afinn_mean,sunmonth5$month.y,sunmonth5$'Total hours',
            xlab = "Date",ylab = "Afinn mean",rylab = "Sunlight hours", xticklab = sunxlab)
twoord.plot(sunmonth5$month.y, sunmonth5$worry_mean,sunmonth5$month.y,sunmonth5$`Total hours`,
            xlab = "Date",ylab = "Worry mean",rylab = "Sunlight hours", xticklab = sunxlab)
# Plot 2 y axis graphs for temp on all 5 samples
# Plot 2 y axis graphs for rain on all 5 samples