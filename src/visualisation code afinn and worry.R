library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)

#DATA CLEANING AND PREPARATION ---------------------------------------------------------

data <- read.csv("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/test.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)

#TOKENISING ----------------------------------------------------------------------------

tokenised_data <- data %>% unnest_tokens(input = text, output = "word", drop = FALSE)

#VISUALISATION -------------------------------------------------------------------------

#afinn lexicon#
afinn <- get_sentiments("afinn") %>%
    mutate(sentiment = ifelse(score>0, "positive", "negative"))

#worry lexicon#
worry <- get_sentiments("nrc") %>% 
               filter(sentiment %in% c("fear", "negative", "sadness")) %>% 
               distinct(word)

#Most common positive/negative words#
afinn_word_counts<- tokenised_data %>%
  inner_join(afinn) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

#Most common worry words#
worry_word_counts<- tokenised_data %>%
  inner_join(worry) %>%
  count(word, sort=TRUE) %>%
  ungroup()

#Show afinn word counts in a plot for frequency greater than 500#
afinn_word_counts %>%
  filter(n > 500) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c('tomato','palegreen1'), guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "grey"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()) +
  ylab("Contribution to sentiment")

#Show worry word counts in a plot for frequency greater than 500#
worry_word_counts %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill)) +
  geom_bar(stat = "identity", fill='paleturquoise') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylab("Contribution to sentiment")

#Show afinn word counts in a word cloud#

tokenised_data %>% 
  inner_join(afinn) %>%
  count(word, sentiment, sort =TRUE) %>%
  acast(word ~ sentiment, value.var ="n", fill=0) %>%
  comparison.cloud(colors=c('tomato','palegreen1'), max.words=50)

#Show worry word counts in a word cloud#

wordcloud (worry_word_counts$word, freq= worry_word_counts$n, max.words=100,
           random.order=FALSE, color = 'paleturquoise3')







