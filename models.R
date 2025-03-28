### loading packages
library(tidyverse)
theme_set(theme_bw())

### Comparison Function
comparison <- function(lms, names, data){
  
  # initialise a dataframe for the results
  results <- data.frame(
    Model = character(),
    R_squared = numeric(),
    Adjusted_R_squared = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    RMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(lms)) {
    model <- lms[[i]]
    model_name <- names[i]
    
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

### loading the csv files as data.frames
demand <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_demand_modelling.csv")
hourly_temp <- read.csv("C:\\Users\\Saioa\\OneDrive - University of Edinburgh\\y3s2\\StatComp\\Project02\\SCS_hourly_temp.csv")

### creating dataframes
# formatting date in the hourly_temp dataframe correctly
hourly_temp <- separate(hourly_temp, Date, c("Date", "Time"), sep = " ")
hourly_temp$Date <- strptime(as.character(hourly_temp$Date), "%d/%m/%Y")
hourly_temp$Date <- format(hourly_temp$Date, "%Y-%m-%d")

# create a dataframe with mean daily temperature
mean_daily_temp <- hourly_temp %>%
  group_by(Date) %>%
  summarise(avg_daily_temp = mean(temp))
head(mean_daily_temp)

# join the two dataframes together by Date
data1 <- left_join(x = demand, y = mean_daily_temp)

### Ella's lovely models

m_0 <- lm(demand_gross ~ wind + solar_S + temp + factor(wdayindex) + factor(monthindex), data = demand)

m_0_year <- lm(demand_gross ~ wind + solar_S + temp + factor(wdayindex) + factor(monthindex) + year, data = demand)

m_0_year_cubed <- lm(demand_gross ~ wind + solar_S + temp + factor(wdayindex) + factor(monthindex) + poly(year, 3), data = demand)

m_0_year_to <- lm(demand_gross ~ wind + solar_S + TO + factor(wdayindex) + factor(monthindex) + year, data = demand)

m_0_year_te <- lm(demand_gross ~ wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + year, data = demand)

m_0_year_squared <- lm(demand_gross ~ (wind + solar_S + temp + factor(wdayindex) + factor(monthindex) + year)^2, data = demand)

m_0_year_to_squared <- lm(demand_gross ~ (wind + solar_S + TO + factor(wdayindex) + factor(monthindex) + year)^2, data = demand)

m_0_year_te_squared <- lm(demand_gross ~ (wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + year)^2, data = demand)

m_0_year_cubed_te <- lm(demand_gross ~ wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + poly(year,3), data = demand)

m_0_year_cubed_te_squared <- lm(demand_gross ~ (wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + poly(year, 3))^2, data = demand)

m_0_year_cubed_te_squared_backward <- lm(demand_gross ~ wind + solar_S + TE + factor(wdayindex) + factor(monthindex) + 
                                           poly(year, 3) + wind:factor(monthindex) + wind:poly(year, 3) + solar_S:factor(wdayindex) + solar_S:factor(monthindex) + 
                                           TE:factor(monthindex) + TE:poly(year, 3) + factor(wdayindex):factor(monthindex) + 
                                           factor(wdayindex):poly(year, 3) + factor(monthindex):poly(year, 3), data = demand)


## performing comparison
models <- list(m_0, m_0_year, m_0_year_cubed, m_0_year_to, m_0_year_te, m_0_year_squared, m_0_year_to_squared, m_0_year_te_squared, m_0_year_cubed_te, m_0_year_cubed_te_squared, m_0_year_cubed_te_squared_backward)
names <- c("Basic Model", "With Year", "With Year^3", "With Year and TO", "With Year and TE", "All Squared", "All Squared with TO", "All Squared with TE", "With Year^3", "With Year^3, TE and All Squared", "Backward")
comparison_df <- comparison(models, names, data1)
comparison_df

## backward stepwise regression
backward_model <- step(m_0_year_cubed_te_squared, direction = "backward")
