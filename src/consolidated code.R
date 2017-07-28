library(rtweet)
library(tidyverse)
library(tidytext)

#Usage: process_data(filepath, output)
#Calling process_data(filepath, output), and replacing filepath with the location of the 
#file you're looking to "sentimise", returns the file, to a new location specified (as output),
#but with an AFINN and worry score attached.
#e.g. process_data("C:\\Users\\Eleanor Martin.DESKTOP-2EC17IB\\Documents\\chunck0.uniq.csv",
# "C:\\Users\\Eleanor Martin.DESKTOP-2EC17IB\\Documents\\test.csv")


process_data <- function(filepath,output){

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

worry_scored_data <- tokenised_data %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("fear", "negative", "sadness")) %>% 
               distinct(word), 
             by = "word") %>%
  group_by(ID) %>%
  summarise(worry = n()) %>%
  full_join(cleaned_data, by = "ID") %>%
  mutate(worry= ifelse (is.na(worry),0,worry))

scored_data_final <- tokenised_data %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(ID) %>%
  summarise(afinn = sum(score)) %>% 
  full_join(worry_scored_data, by = "ID") %>%
  mutate(afinn= ifelse (is.na(afinn),0,afinn))

#OUTPUT --------------------------------------------------------------------------------

write.csv(scored_data_final, 
          output,
          row.names = FALSE)

}

process_data("C:\\Users\\Eleanor Martin.DESKTOP-2EC17IB\\Documents\\chunck0.uniq.csv",
             "C:\\Users\\Eleanor Martin.DESKTOP-2EC17IB\\Documents\\test.csv")

