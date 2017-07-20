# install from CRAN
install.packages("rtweet")
install.packages("httpuv")
install.packages("tidyverse")
install.packages("tidytext")

# load rtweet tidyverse tidytext
library(rtweet)
library(tidyverse)
library(tidytext)

#download tweets from the API
appname <- "rtweet_tokens_steel"

## api key (example below is not a real key)
key <- "C4CVpjdTkYqW7gr3VRqdoBv9V"

## api secret 
secret <- "***"

## create token named "twitter_token"
twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret)

##stream tweets @12:01pm for 30 seconds
tweets_data <- stream_tweets("", timeout = 30)
View(tweets_data)

##cleaning data - keep only the required variables
clean_tweets_data <- tweets_data %>% 
  select(screen_name, 
         created_at, 
         status_id,
         text, 
         is_retweet,
         lang) %>%
  unnest_tokens(input = text, output = "word", drop = FALSE) ##take sentence, split into words and remove symbols

##sentiment analysis to identify most common words with associated sentiment
##sentiment analysis using nrc
sentiment <- get_sentiments("nrc")

clean_tweets_data %>%
  semi_join(sentiment) %>%
  count(word, sort = TRUE)

##sentiment analysis using afinn
sentiment <- get_sentiments("afinn")

clean_tweets_data %>%
  inner_join(sentiment) %>%
  group_by(screen_name, created_at, status_id, text) %>%
  summarise(afinn = sum(score)) %>%
  View

clean_tweets_data %>%
  inner_join(sentiment) %>%
  summarise(sum(score))
  
##sentiment analysis using BING

sentiment <- get_sentiments("bing")

clean_tweets_data %>%
  inner_join(sentiment) %>% 
  group_by(screen_name, created_at, status_id, text) %>% 
  mutate(sentiment_score = case_when(sentiment == "positive" ~ 1, 
                                     sentiment == "negative" ~ -1)) %>%
  summarise(bing = sum(sentiment_score)) %>%
  View

clean_tweets_data %>%
  inner_join(sentiment) %>%
   mutate(sentiment_score = case_when(sentiment == "positive" ~ 1, 
                                     sentiment == "negative" ~ -1)) %>%
  summarise(sum(sentiment_score))


