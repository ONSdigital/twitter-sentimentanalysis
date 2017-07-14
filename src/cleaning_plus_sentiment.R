#This script will take a data.frame of tweets, tokenize the tweets into a tidy format and clean unwanted punctuation.
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
#Create test data.

tweets<-read.csv("C:/Users/Joseph Jenkins/Documents/twit_13Aug14_1Ksample.csv", header = F, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)%>%
  select(V3, V4, V6, V7, V8, V9, V10)%>%
  filter(V4 =="en" & V8 !="NA" & V9 !="NA" & V3 !="")

#function to clean tweet
Clean_Tweet<-function(tweets){
clean_tweet <- gsub("&amp", "", tweets$V10) #remove ampersand
clean_tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) #remove retweets 
clean_tweet <- gsub("@\\w+", "", clean_tweet) #remove mentions
clean_tweet <- gsub("[[:punct:]]", "", clean_tweet) #remove punctuation
clean_tweet <- gsub("[[:digit:]]", "", clean_tweet) #remove numbers
clean_tweet <- gsub("http\\w+", "", clean_tweet) # remove anything starting with http
clean_tweet <- gsub("[ \t]{2,}", "", clean_tweet) #remove tabs
clean_tweet <- gsub("^\\s+|\\s+$", "", clean_tweet)  
}
#run function
cleaned_tweets<-Clean_Tweet(tweets)
#combine cleaned tweets with original tweet data (including ID, location etc)
ready_tweets<-as.data.frame(cleaned_tweets, stringsAsFactors = FALSE)
output<-cbind(ready_tweets, tweets)

#Tokenize tweets into tidy dataframe.
tokenized_tweets <- output %>% 
  unnest_tokens(input = cleaned_tweets, output = word) %>%
  anti_join(stop_words)

#import lexicons and create +ive and -ive lexicon

pos<-get_sentiments(lexicon = "bing")%>%
  filter(sentiment == "positive")

neg<-get_sentiments(lexicon = "bing")%>%
  filter(sentiment == "negative")

#carry out sentiment analysis
neg_sentiment<-tokenized_tweets%>%
  inner_join(neg)%>%
  count(word, sort=TRUE)

pos_sentiment<-tokenized_tweets%>%
  inner_join(pos)%>%
  count(word,sort=TRUE)

tweet_sentiment<-tokenized_tweets%>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)





