demand <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_demand_modelling.csv")
hourly_temp <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_hourly_temp.csv")

## loading packages
library(tidyverse)
theme_set(theme_bw())

## loading the data
head(demand)
head(hourly_temp)

## reorganise and make into one dataframe
# hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
# hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
# hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")
data1 <- demand

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
  
  # creating dataframes of predictions from each linear model
  p_basic <- data.frame(meanA = pred_basic$mean, sdA = pred_basic$sd, lwrA = pred_basic$lwr, uprA = pred_basic$upr)
  p_year <- data.frame(meanB = pred_year$mean, sdB = pred_year$sd, lwrB = pred_year$lwr, uprB = pred_year$upr)
  
  
  return(list(pred_basic = p_basic, pred_year = p_year))
}

# Function to compute error metrics
compute_predictive_scores <- function(actual, mean_pred, sd_pred) {
  se <- (actual - mean_pred)^2
  ds <- (actual - mean_pred)^2 / sd_pred^2 + 2 * log(sd_pred)
  mae <- abs(actual - mean_pred)  # Mean Absolute Error
  rae <- abs(actual - mean_pred) / abs(actual)  # Relative Absolute Error
  sr  <- (actual - mean_pred) / sd_pred  # Standardized Residual
  log_score <- -0.5 * log(2 * pi * sd_pred^2) - (se / (2 * sd_pred^2))  # Log-Score
  
  return(data.frame(
    mean_se = mean(se, na.rm = TRUE),
    mean_ds = mean(ds, na.rm = TRUE),
    mean_mae = mean(mae, na.rm = TRUE),
    mean_rae = mean(rae, na.rm = TRUE),
    mean_sr = mean(sr, na.rm = TRUE),
    mean_log_score = mean(log_score, na.rm = TRUE)
  ))
}
  
# Function to perform LOOCV for a given formula and dataset
loocv_model <- function(data, formula) {
  data <- data %>%
    group_by(monthindex) %>%
    group_modify(~ {
      df <- .x
      n <- nrow(df)
      
      # Initialize storage vectors 
      mean_pred <- rep(NA_real_, n)
      sd_pred <- rep(NA_real_, n)
      
      for (i in seq_len(n)) {
        # Fit model leaving out the i-th observation
        fit <- lm(formula, data = df[-i, , drop = FALSE])
        
        # Predict for the left-out observation
        pred <- predict(fit, newdata = df[i, , drop = FALSE], se.fit = TRUE, interval = "prediction", level = 0.95)
        
        # Store mean prediction and standard deviation
        mean_pred[i] <- pred$fit[,"fit"]
        sd_pred[i] <- sqrt(pred$se.fit^2 + summary(fit)$sigma^2)
      }
      
      df$mean_pred <- mean_pred
      df$sd_pred <- sd_pred
      return(df)
    }) %>%
    ungroup()
  
  return(data)
} 

estimate_model_loocv <- function(data) {
  
  # Define model formulas
  formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex
  formula_year  <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year

  
  # Perform LOOCV for each model
  result_basic <- loocv_model(data, formula_basic)
  result_year  <- loocv_model(data, formula_year)

  error_basic <- compute_predictive_scores(data$demand_gross, result_basic$mean_pred, result_basic$sd_pred)
  error_year  <- compute_predictive_scores(data$demand_gross, result_year$mean_pred, result_year$sd_pred)
  
  # Combine results into the dataset
  results_df <- data.frame(
    Model = c("Basic Model", "Year Model"),
    Mean_SE = c(error_basic$mean_se, error_year$mean_se),
    Mean_DS = c(error_basic$mean_ds, error_year$mean_ds),
    Mean_MAE = c(error_basic$mean_mae, error_year$mean_mae),
    Mean_RAE = c(error_basic$mean_rae, error_year$mean_rae),
    Mean_SR = c(error_basic$mean_sr, error_year$mean_sr),
    Mean_Log_Score = c(error_basic$mean_log_score, error_year$mean_log_score)
  )
  
  return(results_df)
}

# Apply LOOCV estimation to `data1`
results_df <- estimate_model_loocv(data1)

# View the first few rows of results
print(results_df)

