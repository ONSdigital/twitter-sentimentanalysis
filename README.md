# twitter-sentimentanalysis

An R based system for measuring sentiment analysis on tweets.

![mrworry](images/mr_worry.png)
# Mr Worry

MR. MEN™ LITTLE MISS™ Copyright © 2017 THOIP

# Data Cleanup
Data from the API underwent further cleanup in Bash to remove erroneuos rows, whereby tweets had been concatenated.

`grep "[0-9]$" INPUT.csv | grep "^GB" > output.csv`
