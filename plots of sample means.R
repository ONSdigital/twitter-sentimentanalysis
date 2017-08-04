
afinn_mean_merge <- data.frame(means_by_date_1$afinn_mean, means_by_date_2$afinn_mean, means_by_date_3$afinn_mean, means_by_date_4$afinn_mean, means_by_date_5$afinn_mean)

worry_mean_merge <- data.frame(means_by_date_1$worry_mean, means_by_date_2$worry_mean, means_by_date_3$worry_mean, means_by_date_4$worry_mean, means_by_date_5$worry_mean)


plot(worry_mean_merge$means_by_date_1.worry_mean,type = "o",col = "red", xlab = "week", ylab = "worry_mean", 
     main = "worry by week")

lines(worry_mean_merge$means_by_date_2.worry_mean, type = "o", col = "blue")  
lines(worry_mean_merge$means_by_date_3.worry_mean, type = "o", col = "orange")
lines(worry_mean_merge$means_by_date_4.worry_mean, type = "o", col = "green")
lines(worry_mean_merge$means_by_date_5.worry_mean, type = "o", col = "black")

plot(afinn_mean_merge$means_by_date_1.afinn_mean,type = "o",col = "red", xlab = "week", ylab = "afinn_mean", 
     main = "afinn by week")

lines(afinn_mean_merge$means_by_date_2.afinn_mean, type = "o", col = "blue")  
lines(afinn_mean_merge$means_by_date_3.afinn_mean, type = "o", col = "green") 
lines(afinn_mean_merge$means_by_date_4.afinn_mean, type = "o", col = "red") 
lines(afinn_mean_merge$means_by_date_5.afinn_mean, type = "o", col = "black") 

