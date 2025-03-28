demand <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_demand_modelling.csv")
# hourly_temp <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_hourly_temp.csv")

## loading packages
library(tidyverse)
theme_set(theme_bw())

## loading the data
head(demand)
# head(hourly_temp)

## reorganise and make into one dataframe
# hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
# hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
# hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")
data1 <- demand

## check the new dataframe
head(data1)

## implementing the basic model
# lm_basic <- lm(demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex, data = data1)


# Function to compute error metrics
# compute_predictive_scores <- function(df) {
#   df %>%
#     mutate(
#       se = (.[[target_var]] - mean_pred)^2,
#       ds = (.[[target_var]] - mean_pred)^2 / sd_pred^2 + 2 * log(sd_pred),
#       mae = abs(.[[target_var]] - mean_pred),
#       rae = abs(.[[target_var]] - mean_pred) / abs(.[[target_var]]),
#       sr = (.[[target_var]] - mean_pred) / sd_pred,
#       log_score = -0.5 * log(2 * pi * sd_pred^2) - se / (2 * sd_pred^2)
#     ) %>%
#     group_by(monthindex) %>%
#     summarise(
#       mean_se = mean(se, na.rm = TRUE),
#       mean_ds = mean(ds, na.rm = TRUE),
#       mean_mae = mean(mae, na.rm = TRUE),
#       mean_rae = mean(rae, na.rm = TRUE),
#       mean_sr = mean(sr, na.rm = TRUE),
#       mean_log_score = mean(log_score, na.rm = TRUE),
#       .groups = "drop"
#     )
# }

  

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
  formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex
  formula_year  <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year
  
  # Perform LOOCV for each model using correct logic
  result_basic <- loocv_model(data, formula_basic)
  result_year  <- loocv_model(data, formula_year)
  
  # Add model label
  result_basic$model <- "Basic"
  result_year$model  <- "Year"
  
  # Combine both results
  results_all <- bind_rows(result_basic, result_year)
  
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
meanpred <- result_basic$mean_pred
actual <- result_basic$actual
