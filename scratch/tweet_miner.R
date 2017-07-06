library(rtweet)
library(tidytext)
library(tidyverse)

appname <-
key <- 
secret <- 

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

data <- stream_tweets(timeout = 30) #Change this, can set to Inf

tokenised_data <- data %>% 
  select(screen_name, created_at, text, is_retweet) %>%
  filter(is_retweet == FALSE) %>%
  unnest_tokens(input = text, output = word, token = "regex")

tokenised_data %>% View
  
  






## df <- tibble(user = "rossbowen01", text = "<U+124><U+6543>I am @rossbowen01 #hello")
## 
## df %>% 
##   unnest_tokens(input = text, output = word, token = function(x) stri_extract_all(x, regex = "<U\\+\\d+>|.+")) %>%
##   unnest_tokens(input = word, output = word, token = "regex")
