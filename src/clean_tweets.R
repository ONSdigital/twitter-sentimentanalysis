#This script will take a data.frame of tweets, tokenize the tweets into a tidy format and clean unwanted punctuation.

library(tidyverse)
library(tidytext)

#Create test data.

tweets <- tibble(user = c("rossbowen01", "alanevans1"), tweet_text = c("This! is a test, #tweet? @alanevans1", "this_is ;also,a a.test twe3t"))

#Tokenize tweets into tidy dataframe.

tokenized_tweets <- tweets %>% 
  unnest_tokens(input = tweet_text, output = word) %>%
  anti_join(stop_words)
