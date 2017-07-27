library(rtweet)
library(tidyverse)
library(tidytext)

#Usage: process_data(filepath)
#Calling process_data(filepath), and replacing filepath with the location of the 
#file you're looking to "sentimise", returns the file, to the same location specified,
#but with an AFINN score attached.
#e.g. process_data("C:\\Users\\Ross Bowen\\Documents\\chunck0.uniq.csv")

process_data <- function(filepath){

#DATA CLEANING AND PREPARATION ---------------------------------------------------------

data <- read.csv(filepath, 
                 header = TRUE, 
                 stringsAsFactors = FALSE)

data <- data %>% 
  filter(language == "en") %>%
  mutate(ID = row_number())

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

cleaned_data <- data %>% mutate(text = clean_tweet(text))

#TOKENISING ----------------------------------------------------------------------------

tokenised_data <- cleaned_data %>% unnest_tokens(input = text, output = "word", drop = FALSE)

#SENTIMENT SCORING ---------------------------------------------------------------------

scored_data <- tokenised_data %>%
  inner_join(bind_rows(get_sentiments("nrc"), get_sentiments("afinn")), by = "word") %>%
  group_by(ID, word) %>%
  summarise(afinn = sum(score, na.rm = TRUE),
            anger = sum(sentiment == "anger", na.rm = TRUE),
            anticipation = sum(sentiment == "anticipation", na.rm = TRUE),
            disgust = sum(sentiment == "disgust", na.rm = TRUE),
            fear = sum(sentiment == "fear", na.rm = TRUE),
            joy = sum(sentiment == "joy", na.rm = TRUE),
            negative = sum(sentiment == "negative", na.rm = TRUE),
            positive = sum(sentiment == "positive", na.rm = TRUE),
            sadness = sum(sentiment == "sadness", na.rm = TRUE),
            surprise = sum(sentiment == "surprise", na.rm = TRUE),
            trust = sum(sentiment == "trust", na.rm = TRUE),
            condensed_worry = any(sentiment %in% c("anger", "disgust", "fear", "negative", "sadness"), na.rm = TRUE)) %>% 
  summarise(afinn = sum(afinn, na.rm = TRUE),
            anger = sum(anger, na.rm = TRUE),
            anticipation = sum(anticipation, na.rm = TRUE),
            disgust = sum(disgust, na.rm = TRUE),
            fear = sum(fear, na.rm = TRUE),
            joy = sum(joy, na.rm = TRUE),
            negative = sum(negative, na.rm = TRUE),
            positive = sum(positive, na.rm = TRUE),
            sadness = sum(sadness, na.rm = TRUE),
            surprise = sum(surprise, na.rm = TRUE),
            trust = sum(trust, na.rm = TRUE),
            condensed_worry = sum(condensed_worry)) %>%
  mutate(worry = anger + disgust + fear + negative + sadness) %>%
  full_join(cleaned_data, by = "ID") %>%
  select(-ID)

#scored_data_bing <- tokenised_data %>%
#  inner_join(get_sentiments("bing")) %>%
#  mutate(score = case_when(sentiment == "positive" ~ 1, 
#                                     sentiment == "negative" ~ -1)) %>%
#  group_by(V2) %>%
#  summarise(bing = sum(score)) %>% full_join(cleaned_data)

#OUTPUT --------------------------------------------------------------------------------

write.csv(scored_data, filepath,
          row.names = FALSE)

}
