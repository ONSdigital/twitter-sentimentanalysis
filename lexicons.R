##A code for creating positive and negative word lists


## Install necessary packages
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("stringr")

# Put the necessary packages in the library
library(dplyr)
library(tidytext)
library(stringr)

# Create the positive word list by referring to the Bing word list in the tidytext package
# get_sentiments calls in the bing list, filter selects only the values of sentiment that are "positive"
# subset selects only the column labelled "word".
pw = subset(filter(get_sentiments("bing"),sentiment=="positive"), select = c("word"))
View(pw)

# Create the negative word list by referring to the Bing word list in the tidytext package
# get_sentiments calls in the bing list, filter selects only the values of sentiment that are "positive"
# subset selects only the column labelled "word".
nw = subset(filter(get_sentiments("bing"),sentiment=="negative"), select = c("word"))
View(nw)
