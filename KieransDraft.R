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
compute_predictive_scores <- function(df, target_var = "demand_gross") {
  df %>%
    mutate(
      se = (.[[target_var]] - mean_pred)^2,
      ds = (.[[target_var]] - mean_pred)^2 / sd_pred^2 + 2 * log(sd_pred),
      mae = abs(.[[target_var]] - mean_pred),
      rae = abs(.[[target_var]] - mean_pred) / abs(.[[target_var]]),
      sr = (.[[target_var]] - mean_pred) / sd_pred,
      log_score = -0.5 * log(2 * pi * sd_pred^2) - se / (2 * sd_pred^2)
    ) %>%
    group_by(monthindex) %>%
    summarise(
      mean_se = mean(se, na.rm = TRUE),
      mean_ds = mean(ds, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      mean_rae = mean(rae, na.rm = TRUE),
      mean_sr = mean(sr, na.rm = TRUE),
      mean_log_score = mean(log_score, na.rm = TRUE),
      .groups = "drop"
    )
}

  

# Function to perform LOOCV for a given formula and dataset
loocv_model <- function(data, formula) {
  # Get all unique months
  all_months <- unique(data$monthindex)
  
  # Initialize list to store results
  results <- list()
  
  # Loop over each month
  for (month in all_months) {
    # Split data into training (all months except current) and test (current month)
    train_data <- droplevels(subset(data, monthindex != month))
    test_data <- subset(data, monthindex == month)
    
    # Fit model on training data
    fit <- tryCatch(
      lm(formula, data = train_data),
      error = function(e) NULL
    )
    
    # Predict on test data
    if (!is.null(fit)) {
      pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
      
      test_data$mean_pred <- pred$fit[, "fit"]
      test_data$sd_pred <- sqrt(pred$se.fit^2 + summary(fit)$sigma^2)
    } else {
      # Fallback if model fails
      test_data$mean_pred <- NA_real_
      test_data$sd_pred <- NA_real_
    }
    
    results[[as.character(month)]] <- test_data
  }
  
  # Combine all results
  result_df <- bind_rows(results)
  return(result_df)
}

estimate_model_loocv <- function(data) {
  
  # Define model formulas
  formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex
  formula_year  <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year

  
  # Perform LOOCV for each model
  result_basic <- loocv_model(data, formula_basic)
  result_year  <- loocv_model(data, formula_year)

  result_basic$demand_gross <- data$demand_gross
  result_year$demand_gross  <- data$demand_gross
  
  # Combine results into the dataset
  scores_basic <- compute_predictive_scores(result_basic)
  scores_year  <- compute_predictive_scores(result_year)
  
  # Add model labels
  scores_basic$model <- "Basic Model"
  scores_year$model  <- "Year Model"
  
  # Combine all results
  scores_all <- bind_rows(scores_basic, scores_year) %>%
    select(model, monthindex, everything()) %>%
    arrange(model, monthindex)
  
  return(scores_all)
}


results_df <- estimate_model_loocv(data1)

# View the first few rows of results
print(results_df)

