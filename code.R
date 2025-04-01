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

comparison <- function(lms, names, datalist){
  
  # Initialise a dataframe for the results
  results <- data.frame(
    Model = character(),
    R_squared = numeric(),
    Adjusted_R_squared = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    RMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over the models
  for (i in 1:length(lms)) {
    
    # Extract models and names from the inputted dataframe
    model <- lms[[i]]
    model_name <- names[i]
    data <- datalist[[i]]
    
    # Calculate R-squared and Adjusted R-squared
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    
    # Calculate AIC and BIC
    model_aic <- AIC(model)
    model_bic <- BIC(model)
    
    # Calculate RMSE (Root Mean Squared Error)
    predictions <- predict(model, newdata = data)
    mse <- mean((data[["demand_gross"]] - predictions)^2)
    rmse <- sqrt(mse)
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(
      Model = model_name,
      R_squared = r_squared,
      Adjusted_R_squared = adj_r_squared,
      AIC = model_aic,
      BIC = model_bic,
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

lm_predicting <- function(lm, data){
  
  # predict using the linear model
  confidence <- predict.lm(object = lm, interval = "confidence", level = 0.95)
  prediction <- predict.lm(object = lm, newdata = data,
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

#' Plotting actual against predicted data
#' 
#' Checking the accuracy of our linear model by predicting on all historical
#' data, then plotting actual against predicted
#' 
#' @param prediction output of function lm_predicting
#' @param data the data that we are modelling on

plotting <- function(prediction, data){

  # putting historical data and estimated data in a dataframe
  data <- cbind(prediction, data)
  
  # plotting demand against date, creating separate plots by month
  pp <- ggplot(data, aes(x = demand_gross, y = mean, fill = daytype)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = daytype), alpha = 0.2) +
    geom_line(aes(color = daytype), size = 1) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    scale_fill_manual(values = c("weekday" = "cyan", "weekend" = "pink")) + # Custom colors
    scale_color_manual(values = c("weekday" = "cyan", "weekend" = "pink"))
  
  return(pp)

}

#' Leave-One-Out Cross Validation
#' 
#' Using the data given and formula to cross validate our best model
#' by splitting my month
#' 
#' @param data the given dataframe
#' @param formula chosen linear model

monthly_loocv_model <- function(data, formula) {
  
  months <- levels(data$month)
  
  # fill dataframe row-by-row with prediction derived from removing a month at a time
  results <- lapply(months, function(test_month) {
    
    # divide 
    train_data <- data %>% filter(month != test_month)
    test_data  <- data %>% filter(month == test_month)
    
    fit <- lm(formula, data = train_data)
    pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
    
    # Extract the predicted mean values, and lower and upper bounds
    mean_pred <- pred$fit[, "fit"]
    lwr_pi <- pred$fit[, "lwr"]
    upr_pi <- pred$fit[, "upr"]
    
    # Filter the predicted means to keep only those greater than 4000
    #mean_pred[mean_pred < 4000] <- NA
    
    data.frame(
      month = test_month,
      actual = test_data$demand_gross,
      mean_pred = mean_pred,
      sd_pred = sqrt(pred$se.fit^2 + summary(fit)$sigma^2),  # Total predictive uncertainty
      lwr_pi = lwr_pi,
      upr_pi = upr_pi
    ) 
  })
  
  bind_rows(results)
}

monthly_loocv_scores <- function(data, results) {
  
  # Compute predictive scores per month and per model
  monthly_scores <- results %>%
    group_by(model, month) %>%
    summarise(
      mean_se = mean((actual - mean_pred)^2),
      mean_ds = mean((actual - mean_pred)^2 / sd_pred^2 + 2 * log(sd_pred), na.rm = TRUE),
      mean_mae = mean(abs(actual - mean_pred), na.rm = TRUE),
      mean_rae = mean(abs(actual - mean_pred) / abs(actual), na.rm = TRUE),
      mean_sr = mean((actual - mean_pred) / sd_pred, na.rm = TRUE),
      mean_log_score = mean(-0.5 * log(2 * pi * sd_pred^2) - 
                              ((actual - mean_pred)^2 / (2 * sd_pred^2)), na.rm = TRUE),
      mean_int = mean(
        (upr_pi - lwr_pi) + 
          (2 / 0.05) * ((lwr_pi - actual) * as.integer(actual < lwr_pi) + (2 / 0.05) * (actual - upr_pi) * as.integer(actual > upr_pi)),
        na.rm = TRUE
      ),
        .groups = "drop"
    )
  
  monthly_scores_df <- as.data.frame(monthly_scores)
  
  return(monthly_scores_df)
}