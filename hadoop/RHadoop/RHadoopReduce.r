#!/usr/bin/env RScript

# We're not actually reducing anything, merely repeating the line output 
# so Hadoop has a reduce step. 

env <- new.env(hash = TRUE)

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  cat(line, "\n", sep="")
}

