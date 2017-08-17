#function to calculate most frequent worry and afinn words and produce plots and wordclouds.
#e.g. plotting("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_scored.csv")
#you can edit the number of words displayed in plots/wordclouds

library(rtweet)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)

#DATA CLEANING AND PREPARATION ---------------------------------------------------------

plotting<-function(input) {
  data <- read.csv(input, 
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

#Show afinn word counts in a plot#
afinnplot <- afinn_word_counts %>%
  filter(n > 1000) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c('tomato','palegreen1'), guide = FALSE) +
  ylab("Contribution to sentiment") +
  ggtitle ("Most common positive and negative words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

print(afinnplot)

#Show worry word counts in a plot#
worryplot <- worry_word_counts %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill)) +
  geom_bar(stat = "identity", fill='paleturquoise') +
  ylab("Contribution to sentiment") +
  ggtitle ("Most common positive and negative words") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
plot(worryplot)

#Show afinn word counts in a word cloud#

tokenised_data %>% 
  inner_join(afinn) %>%
  count(word, sentiment, sort =TRUE) %>%
  acast(word ~ sentiment, value.var ="n", fill=0) %>%
  comparison.cloud(colors=c('tomato','palegreen1'), max.words=50)

#Show worry word counts in a word cloud#

wordcloud (worry_word_counts$word, freq= worry_word_counts$n, max.words=100,
           random.order=FALSE, color = 'paleturquoise3')
}


plotting("C:/Users/Eleanor Martin.DESKTOP-2EC17IB/Documents/total_output_scored.csv")





