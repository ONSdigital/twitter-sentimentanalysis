# twitter-sentimentanalysis

An R based system for measuring sentiment analysis on tweets. Sentiment is measured both in terms of afinn (+/-ive sentiment) and worry
(a combination of fear, sadness and negative lexicons). 

Functions are as follows:

#clean_and_sentiment_score_function:

Takes tweet data as provided and cleans the tweet text to remove various formatting issues (i.e. numbers, retweets, hashtags etc).
Then tokenises the text, and calculates an afinn and worry score, appends it to the twitter data and exports a .csv

#word_clouds_function

Takes the output .csv file from clean_and_sentiment_score_function and produces wordclouds showing the most prevalent +/-ive words, and the
most prevalent worry words.

#afinn_by_date_function

Calculates mean afinn by time period (day/month/day of week etc) as specified by the user, and produces plots.

#worry_by_date_function

The same as afinn_by_date_function, but using mean worry scores  


![mrworry](images/mr_worry.png)
# Mr Worry

MR. MEN™ LITTLE MISS™ Copyright © 2017 THOIP

# Data Cleanup
Data from the API underwent further cleanup in Bash to remove erroneuos rows, whereby tweets had been concatenated.

`grep "[0-9]$" INPUT.csv | grep "^GB" > output.csv`
