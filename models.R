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

best_no_factor <- lm(demand_gross ~ (wind + solar_S + TE + poly(wdayindex, 2) + monthindex + poly(year, 3))^2, data = demand)
summary(best_no_factor)

# Remove extreme outliers for month 11 and 2
demand_no_outliers <- demand[demand$solar_S < 0.2,]
best_no_outliers <- lm(demand_gross ~ wind + solar_S + TE + 
                         poly(wdayindex, 2) + poly(monthindex, 3) + poly(year, 3), 
                       data = demand_no_outliers)
