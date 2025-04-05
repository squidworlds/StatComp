# Group 50
# Place the code needed in the Report_project02.Rmd, including documentation.
#

#' Model Comparison
#'
#' Compare R^2 and other metrics of different linear models for our data
#'
#' @param lms vector of different linear models to compare, all of the form m0 <- lm(formula, data)
#' @param names names of the linear models we are comparing
#' @param datalist a list of the data that we are using for each model in lms

comparison <- function(formulas, names, data){
  
  # Initialise a dataframe for the results
  results <- data.frame(
    Model = character(),
    R_squared = numeric(),
    Adjusted_R_squared = numeric(),
    RMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over the models
  for (i in 1:length(formulas)) {
    
    # Extract models and names from the inputted dataframe
    model <- lm(formulas[[i]], data)
    model_name <- names[i]
    
    # Calculate R-squared and Adjusted R-squared
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    
    # Calculate RMSE (Root Mean Squared Error)
    predictions <- predict(model, newdata = data)
    mse <- mean((data[["demand_gross"]] - predictions)^2)
    rmse <- sqrt(mse)
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(
      Model = model_name,
      R_squared = r_squared,
      Adjusted_R_squared = adj_r_squared,
      RMSE = rmse
    ))
  }
  # Return the comparison data frame
  return(results)
}

#' Historical Modelling
#' 
#' Testing our models on historical data, finding confidence
#' and prediction intervals
#' 
#' @param lm the linear model we are testing
#' @param data the dataset that we are modelling on

lm_predicting <- function(formula, data){
  
  # predict using the linear model
  confidence <- predict.lm(object = lm(formula, data), interval = "confidence", level = 0.95)
  prediction <- predict.lm(object = lm(formula, data), newdata = data,
                           se.fit = TRUE, interval = "prediction", level = 0.95)
  
  # create dataframe with predicted data
  results <- as.data.frame(data)
  
  # find mean, standard deviation and lower & upper bounds for the predicted data
  results$mean_pred <- prediction$fit[, "fit"]
  results$sd <- sqrt(prediction$se.fit^2 + prediction$residual.scale^2)
  results$lwr_pi <- prediction$fit[, "lwr"] - 1.96 * results$sd
  results$upr_pi <- prediction$fit[, "upr"] + 1.96 * results$sd
  results$mean_ci <- confidence[, "fit"]
  results$lwr_ci <- confidence[, "lwr"]
  results$upr_ci <- confidence[, "upr"]
  
  # create a dataframe with the analysed prediction data
  p <- data.frame(mean_pred = results$mean_pred, sd = results$sd, lwr_pi = results$lwr_pi, upr_pi = results$upr_pi, mean_conf = results$mean_ci, lwr_ci = results$lwr_ci, upr_ci = results$upr_ci)
  
  return(p)
}


#' Rolling Cross-Validation
#' 
#' Using a 3-1 year split, based on start_year, the year that each winter in the
#' dataset starts. Predicting the demand for the next year based on the previous
#' 3 years
#' 
#' @param data the dataframe we are modelling over
#' @param formula the formula of the chosen linear model

monthly_rolling_model <- function(data, formula) {
  
  # initialise a dataframe for storing everything
  scores <- data.frame()
  
  # put all the winters in order, removing the first 3 in order to train
  test_years <- sort(unique(data$start_year))[-(1:3)]
  
  # window sizes
  training_window <- 3
  testing_window <- 1
  
  # We want to split data by year
  for (test_year in test_years) {
    
    # split data for training and testing
    train_data <- data %>% filter(start_year >= (test_year - training_window) & start_year < test_year)
    test_data <- data %>% filter(start_year == test_year)
    
    # fit the model on train data, predict on test data
    fit <- lm(formula, data = train_data)
    pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
    
    # observed values of gross demand
    actual <- test_data$demand_gross
    
    # prediction metrics
    mean_pi <- pred$fit[, "fit"]
    sd_pi <- sqrt(pred$se.fit^2 + summary(fit)$sigma^2)
    lwr_pi <- pred$fit[, "lwr"]
    upr_pi <- pred$fit[, "upr"]
    
    # scores
    se_pi <- (actual - mean_pi)^2
    ds_pi <- (actual - mean_pi)^2 / sd_pi^2 + 2 * log(sd_pi)
    int_pi <- upr_pi - lwr_pi + (2 / 0.05) * ((lwr_pi - actual) * as.integer(actual < lwr_pi) + (actual - upr_pi) * as.integer(actual > upr_pi))
    
    # add everything to our dataframe in each iteration of this loop
    new_row <- data.frame(
      year = test_year,
      month = test_data$month,
      actual = actual,
      mean = mean_pi,
      se = se_pi,
      ds = ds_pi,
      int = int_pi,
      sd = sd_pi,
      lwr = lwr_pi,
      upr = upr_pi,
      daytype = test_data$daytype
    )
    
    scores <- rbind(scores, new_row)
  }
  
  return(scores)
  
}

#' Simulating Maximum Demand
#' 
#' Simulating the maximum annual demand for 2013 by using the data from a previous
#' winter season. 
#'
#' @param weather_year the year of the winter season

simulate_max_demand <- function(weather_year) {
 
   # Get weather data for the specific year
  yearly_weather <- demand %>%
    filter(start_year == weather_year) %>%
    dplyr::select(day_month, wind, solar_S, TE)
  
  # Combine with 2013-14 structure
  combined_data <- demand_2013_structure %>%
    left_join(yearly_weather, by = "day_month") 
  
  # Predict demand
  combined_data$predicted_demand <- predict(demand_model, newdata = combined_data)
  
  # Return the maximum predicted demand
  combined_data %>%
    summarise(
      simulated_year = 2013,
      weather_year = weather_year,
      max_demand = max(predicted_demand, na.rm = TRUE))
}

#' Simulating different TE
#' 
#' This function calculates the adjusted \( R^2 \) for different window sizes and compares the performance of models with rolling TE.
#' 
#' @param data the given dataframe
#' @param window_sizes A numeric vector of different window sizes (in days) to be used for calculating the rolling average of the TE variable
calculate_rolling_r2 <- function(data, window_sizes) {# Ensure the Date is in POSIXct format
  data <- data %>%
    mutate(Date = dmy_hm(Date))
  
  # Store results
  results <- data.frame(window = numeric(), adj_r_squared = numeric())
  
  # Loop through the window sizes
  for (window_size in window_sizes) {
    
    # Create the rolling average column for the given window size
    data <- data %>%
      arrange(Date) %>%
      mutate(rolling_TE = rollapply(TE, width = window_size, FUN = mean, align = "right", fill = NA))
    
    # Fit the linear model using the rolling TE
    model <- lm(demand_gross ~ wind + solar_S + rolling_TE + day + month + poly(year, 3), data = data)
    
    # Extract the adjusted R-squared value
    adj_r_squared <- summary(model)$adj.r.squared
    
    # Store the result
    results <- rbind(results, data.frame(window = window_size, adj_r_squared = adj_r_squared))
  }
  
  return(results)
}

