#SENTIMENT ANALYSIS --------------------------------------------------------------------

#AFINN

#Most frequent positive words, afinn
tokenised_data %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(score > 0) %>% 
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"))

#Most frequent negative words, afinn
tokenised_data %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(score < 0) %>% 
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"))

#BING

pos <- get_sentiments(lexicon = "bing") %>%
  filter(sentiment == "positive")

neg <- get_sentiments(lexicon = "bing") %>%
  filter(sentiment == "negative")

neg_sentiment <- tokenized_tweets %>%
  inner_join(neg) %>%
  count(word, sort=TRUE)

pos_sentiment <- tokenized_tweets %>%
  inner_join(pos) %>%
  count(word,sort = TRUE)

tokenized_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


#Week-by-week breakdown of stretch "topics"

#FRIDAY: This code (prior to analysis - i.e., data formation) should be turned into a function 
#(input = .csv, output = .csv with sentiment scores)

#Hadoop - generalise to huge data.


#WORDCLOUD // broken by NRC?
#Geography, differences in UK, linked with poverty/deprivation
#How do you go from coordinates to geographies?
#Scatter plot superimposed on UK map.
#Time series analysis, Fourier analysis - morning/evening, weekday/weekend, holidays etc.

#Identify events circa 2014 which may have influenced sentiment
#ONS Wellbeing metrics comparisons.

#Worried lexicon?

#NRC PCA (which sentiments most separate the population)
#NRC Wellbeing index (weight sentiments according to PCA/Factors)






