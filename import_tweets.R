#install.packages("tidytext")
#install.packages("dplyr")


library(dplyr)
library(tidytext)
library(stringr)

tweets<-read.csv("C:/Users/Joseph Jenkins/Documents/twit_13Aug14_1Ksample.csv", header = F, fileEncoding = "UTF-16LE")%>%
  select(V3, V4, V6, V7, V8, V9, V10)%>%
  filter(V4 =="en" & V8 !="NA" & V9 !="NA")

tweet_text<-tweets%>%
  select(V10)

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"  
tidy_tweets <- tweet_text %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

pos<-get_sentiments(lexicon = "bing")%>%
  filter(sentiment == "positive")

neg<-get_sentiments(lexicon = "bing")%>%
  filter(sentiment == "negative")

