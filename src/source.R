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

data <- read.csv(paste0(filepath), 
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

scored_data_afinn <- tokenised_data %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(ID) %>%
  summarise(afinn = sum(score)) %>% 
  full_join(cleaned_data, by = "ID") %>%
  select(-ID)

#scored_data_bing <- tokenised_data %>%
#  inner_join(get_sentiments("bing")) %>%
#  mutate(score = case_when(sentiment == "positive" ~ 1, 
#                                     sentiment == "negative" ~ -1)) %>%
#  group_by(V2) %>%
#  summarise(bing = sum(score)) %>% full_join(cleaned_data)

#OUTPUT --------------------------------------------------------------------------------

write.csv(scored_data_afinn, filepath,
          row.names = FALSE)

}
