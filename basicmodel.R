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
  lm_basic <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex, data = data1)
  
  predictionA <- predict.lm(object = lm_basic, newdata = filament,
                            se.fit = TRUE, interval = "prediction", level = 0.95)
  
  predA <- as.data.frame(data1)
  predA$mean <- predictionA$fit[,1]
  predA$sd <- sqrt(predictionA$se.fit^2 + predictionA$residual.scale^2)
  predA$lwr <- predictionA$fit[,1] - 1.96 * predA$sd
  predA$upr <- predictionA$fit[,1] + 1.96 * predA$sd
  
  modelB <- lm(Actual_Weight ~ CAD_Weight + Material, data = filament)
  predictionB <- predict.lm(object = modelB, newdata = filament,
                            se.fit = TRUE, interval = "prediction", level = 0.95)
  predB <- as.data.frame(filament)
  predB$mean <- predictionB$fit[,1]
  predB$sd <- sqrt(predictionB$se.fit^2 + predictionB$residual.scale^2)
  predB$lwr <- predictionB$fit[,1] - 1.96 * predB$sd
  predB$upr <- predictionB$fit[,1] + 1.96 * predB$sd
  
  pA <- data.frame(meanA = predA$mean, sdA = predA$sd, lwrA = predA$lwr, uprA = predA$upr)
  pB <- data.frame(meanB = predB$mean, sdB = predB$sd, lwrB = predB$lwr, uprB = predB$upr)
  
  return(list(predA = pA, predB = pB))
}
