## loading the csv files as data.frames

demand <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_demand_modelling.csv")
hourly_temp <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_hourly_temp.csv")

## loading packages
library(tidyverse)
theme_set(theme_bw())

## loading the data
head(demand)
head(hourly_temp)

## reorganise and make into one dataframe
hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")
data1 <- left_join(x = demand, y = hourly_temp)

## check the new dataframe
head(data1)

## implementing the basic model
lm_basic <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex, data = data1)

## creating functions
estimate_model <- function(data1){
  
  # creating the basic linear model
  lm_basic <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex, data = data1)
  
  # predicting using this basic model
  prediction_basic <- predict.lm(object = lm_basic, newdata = data1,
                            se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # analysis for basic model
  pred_basic <- as.data.frame(data1)
  pred_basic$mean <- prediction_basic$fit[,1]
  pred_basic$sd <- sqrt(prediction_basic$se.fit^2 + prediction_basic$residual.scale^2)
  pred_basic$lwr <- prediction_basic$fit[,1] - 1.96 * pred_basic$sd
  pred_basic$upr <- prediction_basic$fit[,1] + 1.96 * pred_basic$sd
  
  # creating a linear model with historical data included
  lm_year <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year, data = data1)
  
  # predicting using year
  prediction_year <- predict.lm(object = lm_year, newdata = data1,
                            se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # analysis for model including year
  pred_year <- as.data.frame(data1)
  pred_year$mean <- prediction_year$fit[,1]
  pred_year$sd <- sqrt(prediction_year$se.fit^2 + prediction_year$residual.scale^2)
  pred_year$lwr <- prediction_year$fit[,1] - 1.96 * pred_year$sd
  pred_year$upr <- prediction_year$fit[,1] + 1.96 * pred_year$sd
  
  # model that takes into account year and time of day
  lm_time <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year + Time, data = data1)
  
  # prediction using time and year
  prediction_time <- predict.lm(object = lm_time, newdata = data1,
                            se.fit = TRUE, interval = "prediction", level = 0.95)
  pred_time <- as.data.frame(data1)
  pred_time$mean <- prediction_time$fit[,1]
  pred_time$sd <- sqrt(prediction_time$se.fit^2 + prediction_time$residual.scale^2)
  pred_time$lwr <- prediction_time$fit[,1] - 1.96 * pred_time$sd
  pred_time$upr <- prediction_time$fit[,1] + 1.96 * pred_time$sd
  
  # creating dataframes of predictions from each linear model
  p_basic <- data.frame(meanA = pred_basic$mean, sdA = pred_basic$sd, lwrA = pred_basic$lwr, uprA = pred_basic$upr)
  p_year <- data.frame(meanB = pred_year$mean, sdB = pred_year$sd, lwrB = pred_year$lwr, uprB = pred_year$upr)
  p_time <- data.frame(meanB = pred_year$mean, sdB = pred_time$sd, lwrB = pred_time$lwr, uprB = pred_time$upr)
  
  return(list(pred_basic = p_basic, pred_year = p_year, pred_time = p_time))
}

