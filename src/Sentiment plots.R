library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)

#Read in the 5 datasets, compute a new variable which identifies sample and merge 5 into final_data
test1 <- read.csv("C:\\Users\\Mattt Steel\\Documents\\test1.csv",
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  mutate(sample=1)

test2 <- read.csv("C:\\Users\\Mattt Steel\\Documents\\test2.csv",
                  header = TRUE, 
                  stringsAsFactors = FALSE) %>%
  mutate(sample=2)

test3 <- read.csv("C:\\Users\\Mattt Steel\\Documents\\test3.csv",
                  header = TRUE, 
                  stringsAsFactors = FALSE) %>%
  mutate(sample=3)

test4 <- read.csv("C:\\Users\\Mattt Steel\\Documents\\test4.csv",
                  header = TRUE, 
                  stringsAsFactors = FALSE) %>%
  mutate(sample=4)

test5 <- read.csv("C:\\Users\\Mattt Steel\\Documents\\test5.csv",
                  header = TRUE, 
                  stringsAsFactors = FALSE) %>%
  mutate(sample=5)

final_data <- bind_rows(test1, test2, test3, test4, test5)

#restructuring date
final_data <- final_data %>% 
  mutate(week=week(ymd(date)))

#create dataset with 2 cols - week and mean worry score
weekly_mean <- final_data %>%
  filter (nchar(text) < 200) %>%
  group_by(sample, week) %>%
  summarise(mean_worry = mean(worry))

weekly_mean <- final_data %>%
  filter (nchar(text) < 200) %>%
  group_by(sample, week) %>%
  summarise(mean_worry = mean(worry))


#plotting
ggplot(weekly_mean, aes(week, mean_worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8)

ggplot(weekly_mean, aes(week, mean_worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8, se=FALSE)

ggplot(weekly_mean, aes(week, mean_worry)) + 
  geom_smooth(alpha=0.8, se=FALSE)

ggplot(weekly_mean, aes(week, mean_worry)) + 
  geom_area(alpha=0.8, se=FALSE)

ggplot(weekly_mean, aes(week, mean_worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8, se=FALSE) +
  geom_point()

ggplot(final_data %>% filter (nchar(text) < 200), aes(week, worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8, se=FALSE) 

#Mean without 0
weekly_mean <- final_data %>%
  filter(worry != 0) %>%
  group_by(sample, week) %>%
  summarise(mean_worry = mean(worry))

ggplot(weekly_mean, aes(week, mean_worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8, se=FALSE)


#Median instead of mean
weekly_median <- final_data %>%
  filter(worry != 0) %>%
  group_by(sample, week) %>%
  summarise(median_worry = median(worry))

ggplot(weekly_median, aes(week, median_worry, group=sample, colour=as.factor(sample), fill=as.factor(sample))) + 
  geom_smooth(alpha=0.8)


