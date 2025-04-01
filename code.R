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
#' Testing our models on historical data
#' 
#' @param lm the linear model we are testing
#' @param data the dataset that we are modelling on

lm_predicting <- function(lm, data){
  
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