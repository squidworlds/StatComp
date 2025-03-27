### loading the csv files as data.frames

demand <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_demand_modelling.csv")
houraly_temp <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_hourly_temp.csv")

### loading packages
library(tidyverse)
theme_set(theme_bw())

### creating functions

## prediction function
predicting <- function(lm, data){
  
  # predict using the linear model
  prediction <- predict.lm(object = lm, newdata = data,
                                se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # create dataframe with predicted data
  pred <- as.data.frame(data)
  
  # find mean, standard deviation and lower & upper bounds for the predicted data
  pred$mean <- prediction$fit[,1]
  pred$sd <- sqrt(prediction$se.fit^2 + prediction$residual.scale^2)
  pred$lwr <- prediction$fit[,1] - 1.96 * pred$sd
  pred$upr <- prediction$fit[,1] + 1.96 * pred$sd
  
  # create a dataframe with the analysed prediction data
  p <- data.frame(mean = pred$mean, sd = pred$sd, lwr = pred$lwr, upr = pred$upr)
  
  return(p)
}

## plotting function
plotting <- function(prediction, data, predictor){
  
  # putting historical data and estimated data in a dataframe
  data <- cbind(data, prediction)
  
  # plotting demand against date, creating separate plots by month
  pp <- ggplot(data) + 
    geom_point(aes(
      x = Date, 
      y = demand_gross,
      col = as.factor(monthindex)
    )) + 
    facet_wrap(~monthindex) +
    labs(col = "Month") +
    ggtitle(paste("Linear model with", predictor)) +
    geom_ribbon(aes(wdayindex, 
                    ymin = lwr, 
                    ymax = upr), 
                alpha = 0.25)
  
}

### loading the data
head(demand)
head(hourly_temp)

## joining dataframes to take mean daily temp

# formatting date in the hourly_temp dataframe correctly
hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")

# create a dataframe with mean daily temperature
mean_daily_temp <- hourly_temp %>%
  group_by(Date) %>%
  summarise(avg_daily_temp = mean(temp))
head(mean_daily_temp)

# join the two dataframes together by Date
data1 <- left_join(x = demand, y = mean_daily_temp)
head(data1)

### applying our functions to the data

## creating models
lm_basic <- lm(demand_gross ~ wind + solar_S + temp + wdayindex + monthindex, data = demand)
lm_year <- lm(demand_gross ~ wind + solar_S + temp + wdayindex + monthindex + year, data = demand)
lm_avgtemp <- lm(demand_gross ~ avg_daily_temp, data = data1)

est_basic <- predicting(lm_basic, demand)
est_year <- predicting(lm_year, demand)
est_avgtemp <- predicting(lm_avgtemp, data1)

pp_basic <- plotting(est_basic, demand, "basic predictors")
pp_basic

pp_year <- plotting(est_year, demand, "year")
pp_year

pp_avgtemp <- plotting(est_avgtemp, data1, "average daily temperature")
pp_avgtemp


## look at R^2 values and adjusted R^2, MSE

## split data into weekdays and weekends
