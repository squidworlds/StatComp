demand <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_demand_modelling.csv")

## loading packages
library(tidyverse)
theme_set(theme_bw())

## loading the data
head(demand)


data1 <- demand
data1$wdayindex <- as.factor(data1$wdayindex)
data1$monthindex <- as.factor(data1$monthindex)
head(data1)

# Function to perform LOOCV for a given formula and dataset
loocv_model <- function(data, formula) {
  unique_months <- unique(data$monthindex)
  
  results <- lapply(unique_months, function(test_month) {
    train_data <- data %>% filter(monthindex != test_month)
    test_data  <- data %>% filter(monthindex == test_month)
    
    fit <- lm(formula, data = train_data)
    pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
    
    tibble(
      monthindex = test_month,
      actual = test_data$demand_gross,
      mean_pred = pred$fit[,"fit"],
      sd_pred = sqrt(pred$se.fit^2 + summary(fit)$sigma^2)  # Total predictive uncertainty
    )
  })
  
  bind_rows(results)
}

estimate_model_loocv <- function(data) {
  
  # Define model formulas
  formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + factor(wdayindex) + factor(monthindex)
  formula_year  <- demand_gross ~ 1 + wind + solar_S + temp + factor(wdayindex) + factor(monthindex) + year
  formula_year_cubed <- demand_gross ~ wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + poly(year, 3)^2
  
# Perform LOOCV for each model using correct logic
  result_basic <- loocv_model(data, formula_basic)
  result_year  <- loocv_model(data, formula_year)
  result_year_cubed <- loocv_model(data, formula_year_cubed)
  
  # Add model label
  result_basic$model <- "Basic"
  result_year$model  <- "Year"
  result_year_cubed$model <- "YearCubed"
  # Combine both results
  results_all <- bind_rows(result_basic, result_year, result_year_cubed)
  # Compute predictive scores per month and per model
  monthly_scores <- results_all %>%
    group_by(model, monthindex) %>%
    summarise(
      mean_se = mean((actual - mean_pred)^2),
      mean_ds = mean((actual - mean_pred)^2 / sd_pred^2 + 2 * log(sd_pred), na.rm = TRUE),
      mean_mae = mean(abs(actual - mean_pred), na.rm = TRUE),
      mean_rae = mean(abs(actual - mean_pred) / abs(actual), na.rm = TRUE),
      mean_sr = mean((actual - mean_pred) / sd_pred, na.rm = TRUE),
      mean_log_score = mean(-0.5 * log(2 * pi * sd_pred^2) - 
                              ((actual - mean_pred)^2 / (2 * sd_pred^2)), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(monthly_scores)
}
monthly_scores <- estimate_model_loocv(data1)
print(monthly_scores)

formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex
result_basic <- loocv_model(data1, formula_basic)

