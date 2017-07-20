library(rtweet)
library(tidyverse)
library(tidytext)

#DATA CLEANING AND PREPARATION ---------------------------------------------------------

data <- read.csv("C:\\Users\\Ross Bowen\\Documents\\twit_13Aug14_1ksample.csv", 
                 header = F, 
                 fileEncoding = "UTF-16LE", 
                 stringsAsFactors = FALSE)

data <- data %>% 
  select(V2:V4, V6:V10) %>% 
  filter(V4 == "en",
         !is.na(V8),
         !is.na(V9)) #Filters to English tweets, which have locational coordinates.

#Function which cleans tweets (maybe shorten later?)
clean_tweet <- function(text){
  clean_tweet <- gsub("&amp", "", text) #remove ampersand
  clean_tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) #remove retweets 
  clean_tweet <- gsub("@\\w+", "", clean_tweet) #remove mentions
  clean_tweet <- gsub("[[:punct:]]", "", clean_tweet) #remove punctuation
  clean_tweet <- gsub("[[:digit:]]", "", clean_tweet) #remove numbers
  clean_tweet <- gsub("http\\w+", "", clean_tweet) # remove anything starting with http
  clean_tweet <- gsub("[ \t]{2,}", "", clean_tweet) #remove tabs
  clean_tweet <- gsub("^\\s+|\\s+$", "", clean_tweet)  
}

cleaned_data <- data %>% mutate(V10 = clean_tweet(V10))

#TOKENISING ----------------------------------------------------------------------------

tokenised_data <- cleaned_data %>% unnest_tokens(input = V10, output = "word", drop = FALSE)

#SENTIMENT SCORING ---------------------------------------------------------------------

scored_data_afinn <- tokenised_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(V2) %>%
  summarise(afinn = sum(score)) %>% full_join(cleaned_data)

scored_data_bing <- tokenised_data %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(score = case_when(sentiment == "positive" ~ 1, 
                                     sentiment == "negative" ~ -1)) %>%
  group_by(V2) %>%
  summarise(bing = sum(score)) %>% full_join(cleaned_data)

#SENTIMENT ANALYSIS --------------------------------------------------------------------

#AFINN

#Most frequent positive words, afinn
tokenised_data %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(score > 0) %>% 
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"))

#Most frequent negative words, afinn
tokenised_data %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(score < 0) %>% 
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"))

#BING

pos <- get_sentiments(lexicon = "bing") %>%
  filter(sentiment == "positive")

neg <- get_sentiments(lexicon = "bing") %>%
  filter(sentiment == "negative")

neg_sentiment <- tokenized_tweets %>%
  inner_join(neg) %>%
  count(word, sort=TRUE)

pos_sentiment <- tokenized_tweets %>%
  inner_join(pos) %>%
  count(word,sort = TRUE)

tokenized_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


#Week-by-week breakdown of stretch "topics"

#FRIDAY: This code (prior to analysis - i.e., data formation) should be turned into a function 
#(input = .csv, output = .csv with sentiment scores)

#Hadoop - generalise to huge data.


#WORDCLOUD // broken by NRC?
#Geography, differences in UK, linked with poverty/deprivation
#How do you go from coordinates to geographies?
#Scatter plot superimposed on UK map.
#Time series analysis, Fourier analysis - morning/evening, weekday/weekend, holidays etc.

#Identify events circa 2014 which may have influenced sentiment
#ONS Wellbeing metrics comparisons.

#Worried lexicon?

#NRC PCA (which sentiments most separate the population)
#NRC Wellbeing index (weight sentiments according to PCA/Factors)






