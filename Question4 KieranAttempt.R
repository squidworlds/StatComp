# Working on Questionn 4
demand <- read.csv("C:\\Users\\kiera\\OneDrive - University of Edinburgh\\Stats Project 2\\Project02\\SCS_demand_modelling.csv")

## loading packages
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
#How could the maximum annual demand have varied in the 2013-14 winter season if different weather
# conditions had occurred? For this you can investigate variation in the maximum annual demand
# in 2013/14 by inputting the weather conditions from previous winters (i.e. test weather in 1991/92,
#                                                                       1992/93, ...)

# First convert character Date to proper date format
demand <- demand %>%
  mutate(
    Date = as.Date(Date),  # Convert character to Date
    day_month = format(Date, "%m-%d"),  # Extract month-day as "MM-DD"
    day_of_month = lubridate::mday(Date) # Eg numbers 1-31
  )

#1. Prepare the base model ----------------------------------------------------
  # Using your original model specification
demand_model <- lm(demand_gross ~ (wind + solar_S + TE + poly(day_of_month, 2) + poly(wdayindex, 2) + poly(monthindex, 3) + poly(start_year, 3))^2, data = demand)

# 2. Prepare the data components -----------------------------------------------

# Create 2013-14 temporal structure (without weather)
demand_2013_structure <- demand %>%
  filter(start_year == 2013) %>%
  dplyr::select(Date, day_month, day_of_month, wdayindex, monthindex, start_year)

# Get historical weather data (all years except 2013-14)
historical_weather <- demand %>%
  filter(start_year < 2013) %>%
  dplyr::select(day_month, wind, solar_S, TE)


# 3. Simulation function -------------------------------------------------------
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

# 4. Run simulations for all historical years ----------------------------------
historical_years <- unique(demand$start_year[demand$start_year < 2013])

# Run simulations (using map_df for efficiency)
simulation_results <- purrr::map_df(historical_years, simulate_max_demand)

# Add actual 2013-14 maximum for comparison
actual_max_2013 <- demand %>%
  filter(start_year == 2013) %>%
  summarise(
    simulated_year = 2013,
    weather_year = 2013,
    max_demand = max(demand_gross, na.rm = TRUE))
simulation_results <- bind_rows(simulation_results, actual_max_2013)    

buffer <- 0.01 * actual_max_2013$max_demand  # 10% buffer
y_min <- min(simulation_results$max_demand, actual_max_2013$max_demand) - buffer
y_max <- max(simulation_results$max_demand, actual_max_2013$max_demand) + buffer

# Plot max_demand for each weather year against actual 2013 max_demand
ggplot(simulation_results, aes(x = as.factor(weather_year), y = max_demand)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot for simulated demand
  geom_hline(yintercept = actual_max_2013$max_demand, color = "red", linetype = "dashed", size = 1) +  # Red dashed line for actual 2013 max_demand
  labs(
    title = "Maximum Annual Demand under Different Weather Conditions",
    x = "Weather Year",
    y = "Maximum Demand (MW)"
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +  # Adjust y-axis range
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


simulation_results <- simulation_results %>%
  mutate(diff_from_2013 = max_demand - actual_max_2013$max_demand)

ggplot(simulation_results, aes(x = weather_year, y = max_demand)) +
  geom_point(shape = 4, color = "blue", size = 3) +  # Scatter points for each weather year
  geom_errorbar(aes(ymin = actual_max_2013$max_demand, ymax = max_demand), width = 0.6, color = "black") +  # Error bars
  geom_hline(yintercept = actual_max_2013$max_demand, color = "red", linetype = "dashed", size = 0.5) +  # Red dashed line for 2013 demand
  labs(
    title = "Maximum Annual Demand under Different Weather Conditions",
    x = "Weather Year",
    y = "Maximum Demand (MW)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels  
