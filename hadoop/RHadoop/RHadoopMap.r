#!/usr/bin/env RScript

source("RUtils.r")

poswords <- LoadPosWordSet()
negwords <- LoadNegWordSet()

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  tweetsplit <- strsplit(line, ",")
  tweet <- tweetsplit[[1]][10]
  sentimentscore = GetScore(tweet, poswords, negwords)
  cat(line, ",", sentimentscore ,"\n", sep="")
}

