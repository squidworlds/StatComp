demand <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_demand_modelling.csv")

## loading packages
library(tidyverse)
library(ggplot2)
library(kableExtra)  # For a nice table


theme_set(theme_bw())

## loading the data
head(demand)



data1 <- demand

# Function to perform LOOCV for a given formula and dataset
loocv_model <- function(data, formula) {
  unique_months <- unique(data$monthindex)
  
  results <- lapply(unique_months, function(test_month) {
    train_data <- data %>% filter(monthindex != test_month)
    test_data  <- data %>% filter(monthindex == test_month)
    
    fit <- lm(formula, data = train_data)
    pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
    
    data.frame(
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
  formula_year_cubed <- demand_gross ~ wind  + TE + poly(wdayindex, 2) + monthindex + poly(year, 3)
  m_0_year_to_squared <- demand_gross ~ wind + solar_S + TO + wdayindex + monthindex + poly(year, 2)
  m_0_year_te_squared <- demand_gross ~ wind + solar_S + TE + wdayindex + monthindex + poly(year, 2)

  # Perform LOOCV for each model using correct logic
  result_basic <- loocv_model(data, formula_basic)
  result_year  <- loocv_model(data, formula_year)
  result_year_cubed <- loocv_model(data, formula_year_cubed)
  result_m_0_to <- loocv_model(data, m_0_year_to_squared)
  result_m_0_te <- loocv_model(data, m_0_year_te_squared)

  # Add model label
  result_basic$model <- "Basic"
  result_year$model <- "Year"
  result_year_cubed$model <- "YearCubed"
  result_m_0_to$model <- "TOSquared"
  result_m_0_te$model <- "TESquared"
  # Combine both results
  results_all <- bind_rows(result_basic, result_year, result_year_cubed, result_m_0_te, result_m_0_to)
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

monthly_scores <- estimate_model_loocv(data2)

monthly_scores %>%
  arrange(model, monthindex) %>%
  kbl(caption = "Monthly Predictive Scores by Model") %>%
  kable_styling(full_width = FALSE)

monthly_scores_long <- monthly_scores %>%
  pivot_longer(cols = c(mean_se, mean_mae, mean_sr, mean_log_score),
               names_to = "metric", values_to = "value")

# Rename metrics for better plot labels
metric_labels <- c(
  mean_se = "Mean Squared Error (MSE)",
  mean_mae = "Mean Absolute Error (MAE)",
  mean_sr = "Standardized Residuals (SR)",
  mean_log_score = "Log Score"
)

# Create a facet grid plot
ggplot(monthly_scores_long, aes(x = as.factor(monthindex), y = value, color = model, group = model)) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~metric, scales = "free_y", labeller = as_labeller(metric_labels)) +
  labs(title = "Comparison of Predictive Scores Across Models",
       x = "Month Index",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

