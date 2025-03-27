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
# we use the hourly_temp dataframe to find the mean temperature per day
hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")
#data1 <- left_join(x = demand, y = hourly_temp)
data1 <- demand

## check the new dataframe
head(data1)
data1 <- na.omit(data1)

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
  
  # # model that takes into account year and time of day
  # lm_time <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year + Time, data = data1)
  # 
  # # prediction using time and year
  # prediction_time <- predict.lm(object = lm_time, newdata = data1,
  #                           se.fit = TRUE, interval = "prediction", level = 0.95)
  # pred_time <- as.data.frame(data1)
  # pred_time$mean <- prediction_time$fit[,1]
  # pred_time$sd <- sqrt(prediction_time$se.fit^2 + prediction_time$residual.scale^2)
  # pred_time$lwr <- prediction_time$fit[,1] - 1.96 * pred_time$sd
  # pred_time$upr <- prediction_time$fit[,1] + 1.96 * pred_time$sd
  
  # creating dataframes of predictions from each linear model
  p_basic <- data.frame(mean_basic = pred_basic$mean, sd_basic = pred_basic$sd, lwr_basic = pred_basic$lwr, upr_basic = pred_basic$upr)
  p_year <- data.frame(mean_year = pred_year$mean, sd_year = pred_year$sd, lwr_year = pred_year$lwr, upr_year = pred_year$upr)
  # p_time <- data.frame(mean_time = pred_time$mean, sd_time = pred_time$sd, lwr_time = pred_time$lwr, upr_time = pred_time$upr)
  
  return(list(pred_basic = p_basic, pred_year = p_year)) # pred_time = p_time
}

# extract the estimations from our functions
est <- estimate_model(data1)
est_basic <- est$pred_basic
est_year <- est$pred_year
# est_time <- est$pred_time

# put the observed and predicted data together in a dataframe
data_basic <- cbind(data1, est_basic)
data_year <- cbind(data1, est_year)
# data_time <- cbind(data1, est_time)

# creating plots
pp_basic <- ggplot(data_basic) + 
  geom_point(aes(
    x = Date, 
    y = demand_gross,
    col = as.factor(monthindex)
    )) + 
  facet_wrap(~monthindex) +
  labs(title = "Basic model", col = "Month") +
  geom_ribbon(aes(wdayindex, 
                  ymin = lwr_basic, 
                  ymax = upr_basic), 
              alpha = 0.25)

pp_year <- ggplot(data_year) + 
  geom_point(aes(
    x = Date, 
    y = demand_gross,
    col = as.factor(monthindex)
  )) + 
  facet_wrap(~monthindex) +
  labs(title = "Model with Year", col = "Month") +
  geom_ribbon(aes(wdayindex, 
                  ymin = lwr_year, 
                  ymax = upr_year), 
              alpha = 0.25)

# pp_time <- ggplot(data_time) + 
#   geom_point(aes(
#     x = wdayindex, 
#     y = Actual_Weight,
#     col = Material
#   )) + 
#   geom_ribbon(aes(wdayindex, 
#                   ymin = lwr_time, 
#                   ymax = upr_time), 
#               alpha = 0.25)

pp_basic
pp_year
