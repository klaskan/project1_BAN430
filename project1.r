library(fpp2)
library(GGally)
library(forecast)
library(ggplot2)

#Wipe console
cat("\014")
#Clean environment
rm(list=ls())
#Set wd
setwd("/Users/")
#csv to df object
df <- read.csv(file="month_aus_prod_chocolate.csv", header=TRUE, sep=",")
#df object to ts object
chocolate_prod <- ts(df[,1], start = c(1957, 7), frequency = 12)


#verifying and exploring the ts #overlaying the appropriate logistic curve
chocolate_prod
autoplot(chocolate_prod, 
         main="Monthly production of chocolate confectionery in Australia: tonnes.\nJuly 1957 - Aug 1995", 
         ylab = "Metric tons")


#Summarizing statistical data  
summary(chocolate_prod)

#overlaying the plot with appropriate logistic curve 
autoplot(chocolate_prod) +
  geom_smooth()


#Plot that shows seasonality
seasonplot_polar <- ggseasonplot(chocolate_prod, year.labels=FALSE,continuous=TRUE, 
                                 polar = TRUE, 
                                 main = "Seasonal plot polar: chocolate_prod")

seasonplot_non_polar <- ggseasonplot(chocolate_prod, year.labels=FALSE, continuous=TRUE)
plot_grid(seasonplot_polar, seasonplot_non_polar, labels = "AUTO")

#Plotting ggACF
ggAcf(chocolate_prod, lag=200)


#STL decomposition
chocolate_prodSTL <- stl(chocolate_prod, s.window="periodic", robust=TRUE, na.action = na.omit)
autoplot(chocolate_prodSTL, main="STL chocolate_prod")


#making a training set + test set 
training_chocolate_prod <- window(chocolate_prod, end=c(1994,8))
testset_94_95 <- window(chocolate_prod, start=c(1994, 9))

#=======STL=========

#12 month naive forecast with STL
fc_STL_naive <- stlf(training_chocolate_prod, method="naive", h=12)

autoplot(fc_STL_naive, xlim=c(1993,1996), xlab = "Time", ylab = "Metric tons" ) +
  autolayer(testset_94_95, series = "Test data") 

accuracy(fc_STL_naive, chocolate_prod)

autoplot(fc_STL_naive, xlim=c(1980,1996), xlab = "Time", ylab = "Metric tons" ) 


# 12-month random walk drift forecast with STL
fc_STL_rwdrift <- stlf(training_chocolate_prod, method="rwdrift", h=12)

autoplot(fc_STL_rwdrift, xlim=c(1993,1996), xlab = "Time", ylab = "Metric tons" ) +
  autolayer(testset_94_95, series = "Test data") 

accuracy(fc_STL_rwdrift, chocolate_prod)


#12 month ets forecast with STL
fc_STL_ets <- stlf(training_chocolate_prod, method="ets", h=12)

autoplot(fc_STL_ets, xlim=c(1993,1996), xlab = "Time", ylab = "Metric tons" ) +
  autolayer(testset_94_95, series = "Test data") 

accuracy(fc_STL_ets, chocolate_prod)



#=======ETS========

#Setting up the ETS model & checking parameters
ets_chco <- ets(training_chocolate_prod)
summary(ets_chco)

#seting up ets forecast and checking residuals
ets_fc <- forecast(ets_chco, h = 12)
checkresiduals(ets_fc)

accuracy(ets_fc, chocolate_prod)

#Plotting forecast
autoplot(ets_fc, xlim=c(1993,1996), xlab = "Time", ylab = "Metric tons" ) +
  autolayer(testset_94_95, series = "Test data")



#========TSCV========
tsCV_2step <- function(){
  #csv to df object
  df <- read.csv(file="month_aus_prod_chocolate.csv", header=TRUE, sep=",")
  #Time series
  chocolate_prod <- ts(df[,1], start = c(1957, 7), frequency = 12)
  #test set 
  test_set <- window(chocolate_prod, start = c(1993, 8))
  
  t <- 0
  merged_error <- c()
  for (i in 1:length(test_set)){
    t <- t + 1
    fc_snaive_value <- snaive(subset(chocolate_prod, end = (433 + t) , h=2)) #forecast
    #merging of error values
    merged_error <- rbind(merged_error, (test_set[2 + t] - fc_snaive_value$mean[2]))
  }
  return(merged_error)
}
merged_error <- tsCV_2step()
merged_error