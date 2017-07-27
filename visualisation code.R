library(rtweet)
library(tidyverse)
library(tidytext)

#DATA CLEANING AND PREPARATION ---------------------------------------------------------

data <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/chunck0.uniq.csv", 
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

#VISUALISATION -------------------------------------------------------------------------

#afinn lexicon#
afinn <- get_sentiments("afinn") %>%
    mutate(sentiment = ifelse(score>0, "positive", "negative"))

#Most common positive/negative words#
afinn_word_counts<- tokenised_data %>%
  inner_join(afinn) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

#Show word counts in a plot for frequency greater than 500#
afinn_word_counts %>%
  filter(n > 500) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#Show word counts in a word cloud#
library(wordcloud)
library (reshape2)

tokenised_data %>% 
  inner_join(afinn) %>%
  count(word, sentiment, sort =TRUE) %>%
  acast(word ~ sentiment, value.var ="n", fill=0) %>%
  comparison.cloud(colors=c("red","green"), max.words=50)

#show frequency of affin scores

library(ggplot2)

scored_data_afinn %>%
  filter(afinn>-20 & afinn<20)%>%
  ggplot(aes(x= afinn)) +
  geom_bar(fill="#880011") +
  labs(x="afinn")








