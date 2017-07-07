library(dplyr)

tweets<-read.csv("C:/Users/Joseph Jenkins/Documents/twit_13Aug14_1Ksample.csv", header = F, fileEncoding = "UTF-16LE")%>%
  select(V3, V4, V6, V7, V8, V9, V10)%>%
  filter(V4 =="en" & V8 !="NA" & V9 !="NA")


