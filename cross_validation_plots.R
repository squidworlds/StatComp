demand <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_demand_modelling.csv")

## loading packages
library(tidyverse)
library(ggplot2)

theme_set(theme_bw())

# Function to perform LOOCV for a given formula and dataset
loocv_model <- function(data, formula) {
  
  unique_months <- unique(data$monthindex)
  
  results <- lapply(unique_months, function(test_month) {
    train_data <- data %>% filter(monthindex != test_month)
    test_data  <- data %>% filter(monthindex == test_month)
    
    fit <- lm(formula, data = train_data)
    pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
    # Extract the predicted mean values
    mean_pred <- pred$fit[,"fit"]
    
    # Filter the predicted means to keep only those greater than 4000
    #mean_pred[mean_pred < 4000] <- NA
    
    data.frame(
      monthindex = test_month,
      actual = test_data$demand_gross,
      mean_pred = mean_pred,
      sd_pred = sqrt(pred$se.fit^2 + summary(fit)$sigma^2)  # Total predictive uncertainty
    ) 
  })
  
  bind_rows(results)
}

estimate_model_loocv <- function(data, results) {
  
  # Compute predictive scores per month and per model
  monthly_scores <- results %>%
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

## loading the data
head(demand)


data1 <- demand
head(data1)
# removing month 2
data2 <- data1[data1$monthindex != 2, ]
head(data2)

# Define model formulas
formula_basic <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex
formula_year  <- demand_gross ~ 1 + wind + solar_S + temp + wdayindex + monthindex + year
formula_year_cubed <- demand_gross ~ 1 + wind + solar_S + TE + wdayindex + monthindex + poly(year, 3)^2

# Perform LOOCV for each model using correct logic
result_basic <- loocv_model(data1, formula_basic)
result_year  <- loocv_model(data1, formula_year)
result_year_cubed <- loocv_model(data1, formula_year_cubed)

# Add model label
result_basic$model <- "Basic"
result_year$model <- "Year"
result_year_cubed$model <- "YearCubed"

# Combine both results
results_all <- bind_rows(result_basic, result_year, result_year_cubed)
results_all

monthly_scores <- estimate_model_loocv(data1, results_all)
print(monthly_scores)

# plotting actual and mean vs month for a single model at a time
ggplot(result_year_cubed, aes(x = monthindex)) +
  geom_point(aes(y = actual), color = "blue", alpha = 0.6, size = 3) +  # Actual values
  geom_point(aes(y = mean_pred), color = "red", alpha = 0.6, size = 3)

# plotting actual vs mean for all models
ggplot(results_all, aes(x = actual, y = mean_pred, color = model)) +
  geom_point(alpha = 0.2) +                      # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Line y=x for reference
  labs(
    title = "Actual vs Predicted Demand",
    x = "Actual Demand",
    y = "Predicted Demand",
    subtitle = "Points represent predictions, dashed line represents perfect prediction"
  )

# plotting actual vs mean for our best models
ggplot(result_year_cubed, aes(x = actual, y = mean_pred, color = model)) +
  geom_point(alpha = 0.2) +                      # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Line y=x for reference
  labs(
    title = "Actual vs Predicted Demand",
    x = "Actual Demand",
    y = "Predicted Demand",
    subtitle = "Points represent predictions, dashed line represents perfect prediction"
  )


# Plotting Mean MAE per month for each model
ggplot(monthly_scores, aes(x = monthindex, y = mean_mae, color = model)) +
  geom_line() +                                  # Line plot for Mean Absolute Error
  geom_point() +                                 # Add points on the line for clarity
  labs(
    title = "Mean Absolute Error (MAE) per Month",
    x = "Month Index",
    y = "Mean MAE",
    subtitle = "Performance of different models over time"
  ) +
  scale_color_manual(values = c("Basic" = "blue", "Year" = "green", "YearCubed" = "red"))

# Plotting Mean RAE per month for each model
ggplot(monthly_scores, aes(x = monthindex, y = mean_rae, color = model)) +
  geom_line() +                                 # Line plot for Mean RAE
  geom_point() +                                # Add points for clarity
  labs(
    title = "Mean Relative Absolute Error (RAE) per Month",
    x = "Month Index",
    y = "Mean RAE",
    subtitle = "Performance of different models over time"
  ) +
  scale_color_manual(values = c("Basic" = "blue", "Year" = "green", "YearCubed" = "red"))

