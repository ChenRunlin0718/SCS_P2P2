library(readr)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(lubridate)
library(patchwork) 
library(knitr)
# ----load dataset----
demand_modelling <- read_csv("SCS_demand_modelling.csv")
hourly_temp <- read_csv("SCS_hourly_temp.csv")
# ----data Cleaning----
# Ensure Date type
demand_modelling$Date <- as.Date(demand_modelling$Date)
# Check NA values
colSums(is.na(demand_modelling))

# Transfer Date column into date and hour
hourly_temp <- hourly_temp %>%
  mutate(
    Hour = hour(dmy_hm(Date)),  # Time
    Date = as.Date(Date, format = "%d/%m/%Y %H:%M") # Date
  )

# ---- Inner joint data sets----
temp_summary <- hourly_temp %>%
  group_by(Date) %>%
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),  # 24-hour average temperature
    max_temp = max(temp, na.rm = TRUE),   # Maximum temperature of the day
    min_temp = min(temp, na.rm = TRUE),   # Minimum temperature of the day
    temp_range = max_temp - min_temp,     # Temperature fluctuation
    
    # Hourly averages calculated correctly
    last_3h_avg = mean(temp[Hour >= 21 & Hour <= 23], na.rm = TRUE),  # Avg temperature 9PM-11PM
    last_6h_avg = mean(temp[Hour >= 18 & Hour <= 23], na.rm = TRUE),  # Avg temperature 6PM-11PM
    last_12h_avg = mean(temp[Hour >= 11 & Hour <= 23], na.rm = TRUE), # Avg temperature 11AM-11PM
    last_18h_avg = mean(temp[Hour >= 6 & Hour <= 23], na.rm = TRUE), # Avg temperature 6AM-11PM
    
    # New variable for evening temperature 6PM-11PM
    evening_temp = mean(temp[Hour >= 18 & Hour <= 23], na.rm = TRUE),
    
    # Daytime & Nighttime averages
    day_temp = mean(temp[Hour >= 6 & Hour <= 18], na.rm = TRUE),   # Avg temperature during the day
    night_temp = mean(temp[Hour < 6 | Hour > 18], na.rm = TRUE)  # Avg temperature during the night
  )
demand_modelling <- inner_join(demand_modelling, temp_summary, by = "Date")
demand_modelling <- demand_modelling %>%
  mutate(wdayindex = as.numeric(as.character(wdayindex)))
# ----Data Cleaning----
# Lag first
demand_modelling <- demand_modelling %>%
  arrange(Date) %>%
  mutate(lag1_demand = lag(demand_gross, 1))


# keep 95% quantile of peak daily demand over a year
demand_modelling_filtered <- demand_modelling %>%
  group_by(year) %>%
  arrange(desc(demand_gross), .by_group = TRUE) %>%
  mutate(row_num = row_number(),
         total_rows = n(),
         keep = row_num <= 0.95 * total_rows) %>%
  filter(keep) %>%
  select(-row_num, -total_rows, -keep)

# Filter out the "Christmas week" and "New Year"
demand_df <- demand_modelling_filtered  %>%
  filter(!(format(Date, "%m-%d") %in% c("01-01", "12-23", "12-24", "12-25", "12-26", "12-27")))


demand_df <- demand_df %>%
  mutate(wdayindex = as.factor(as.character(wdayindex)))
# Convert `start_year` to factor so each winter becomes a dummy,
# using 1990 as the baseline (reference) winter.
demand_df$start_year <- factor(demand_df$start_year)
demand_df$start_year <- relevel(demand_df$start_year, ref = "1991")

# Convert `wdayindex` to factor if you want day-of-week categories.
# We can set Sunday (0) as the reference.
demand_df$wdayindex <- factor(demand_df$wdayindex,
                              levels = c("0","1","2","3","4","5","6"))
demand_df$wdayindex <- relevel(demand_df$wdayindex, ref = "0")


#----- Q1 ----
# ----Compare Variables----
full_model <- lm(
  demand_gross ~ start_year + start_year:TE + wdayindex + solar_S + wind + DSN + I(DSN^2),
  data = demand_df)
summary(full_model)


model_TE <- lm(demand_gross ~ start_year + start_year:TE + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_temp <- lm(demand_gross ~ start_year + start_year:temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_TO <- lm(demand_gross ~ start_year + start_year:TO + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)

model_avg_temp <- lm(demand_gross ~ start_year + start_year:avg_temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_max_temp <- lm(demand_gross ~ start_year + start_year:max_temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_min_temp <- lm(demand_gross ~ start_year + start_year:min_temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_temp_range <- lm(demand_gross ~ start_year + start_year:temp_range + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)

model_3h_avg <- lm(demand_gross ~ start_year + start_year:last_3h_avg + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_6h_avg <- lm(demand_gross ~ start_year + start_year:last_6h_avg + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_12h_avg <- lm(demand_gross ~ start_year + start_year:last_12h_avg + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_18h_avg <- lm(demand_gross ~ start_year + start_year:last_18h_avg + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)

model_day_temp <- lm(demand_gross ~ start_year + start_year:day_temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)
model_night_temp <- lm(demand_gross ~ start_year + start_year:night_temp + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)

model_combine <- lm(demand_gross ~ start_year + avg_temp*temp_range + start_year:TE + wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)


model_performance <- data.frame(
  Model = c("TE", "Temp", "TO", "avg_temp", "max_temp", "min_temp", "temp_range",
            "last_3h_avg", "last_6h_avg", "last_12h_avg", "last_18h_avg",
            "day_temp", "night_temp", "combined model"),
  
  Adj_R2 = c(summary(model_TE)$adj.r.squared, summary(model_temp)$adj.r.squared, summary(model_TO)$adj.r.squared,
             summary(model_avg_temp)$adj.r.squared, summary(model_max_temp)$adj.r.squared, summary(model_min_temp)$adj.r.squared,
             summary(model_temp_range)$adj.r.squared, summary(model_3h_avg)$adj.r.squared, summary(model_6h_avg)$adj.r.squared,
             summary(model_12h_avg)$adj.r.squared, summary(model_18h_avg)$adj.r.squared, 
             summary(model_day_temp)$adj.r.squared, summary(model_night_temp)$adj.r.squared,
             summary(model_combine)$adj.r.squared),
  
  
  AIC = c(AIC(model_TE), AIC(model_temp), AIC(model_TO),
          AIC(model_avg_temp), AIC(model_max_temp), AIC(model_min_temp),
          AIC(model_temp_range), AIC(model_3h_avg), AIC(model_6h_avg),
          AIC(model_12h_avg), AIC(model_18h_avg), 
          AIC(model_day_temp), AIC(model_night_temp), AIC(model_combine)),
  
  MSE = c(summary(model_TE)$sigma^2, summary(model_temp)$sigma^2, summary(model_TO)$sigma^2,
          summary(model_avg_temp)$sigma^2, summary(model_max_temp)$sigma^2, summary(model_min_temp)$sigma^2,
          summary(model_temp_range)$sigma^2, summary(model_3h_avg)$sigma^2, summary(model_6h_avg)$sigma^2,
          summary(model_12h_avg)$sigma^2, summary(model_18h_avg)$sigma^2, 
          summary(model_day_temp)$sigma^2, summary(model_night_temp)$sigma^2,
          summary(model_combine)$sigma^2)
)
# Sort by R² to find the best models
model_performance <- model_performance[order(-model_performance$Adj_R2), ]

kable(model_performance, format = 'latex', col.names = c("Model", "Adjusted R²", "AIC", "MSE"),
      caption = "Comparison of Single-Variable Regression Models for Peak Demand")


#---- Q2-----
# ---- Current Best Model----
model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                 wdayindex + solar_S + wind + DSN + I(DSN^2),
               data = demand_df)
summary(model_TE)
# Residuals Plot
par(mfrow=c(2,2))
plot(model_TE)

# -----Check Autocorrelation
# Residual over time
par(mfrow=c(1,1))
plot(c(1:3221), model_TE$residuals, type="l", xlab="Time", ylab="Residuals")

#' Time series data typically contain some time dependence,
#'  meaning the value at time t is dependent on the value at previous
#'   lags (for example time t − 1, t − 2, . . .). 
#'   Also known as serial correlation or autocorrelation.
#'   With dependent errors the coefficient estimates are still unbiased 
#'   (although not lowest variance as Gauss-Markov won’t hold).
#'   But the standard errors will be wrong...



acf(model_TE$residuals, main="Autocorrelation")


# ---- Lag Model----

#demand_df <- demand_df %>%
#  arrange(Date) %>%
#  mutate(lag1_demand = lag(demand_gross, 1))


model_with_lag <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                       solar_S + wind + DSN + I(DSN^2), data = demand_df)
summary(model_with_lag)
AIC(model_with_lag)
par(mfrow=c(1,1))
acf(model_with_lag$residuals, main="Autocorrelation")
par(mfrow=c(2,2))
plot(model_with_lag)

par(mfrow=c(1,1))
plot(c(1:3220), model_with_lag$residuals, type="l", xlab="Time", ylab="Residuals")

# ----Boostrapping----
# ----Adj_r_square----
library(boot)
# Step 1: Remove the first row
demand_df <- demand_df %>%
  arrange(Date) %>%
  filter(!is.na(lag1_demand))  # Remove first row NA

# Step 2: define boot function
boot_AdjR2 <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE +
                wdayindex + solar_S + wind + DSN + I(DSN^2), data = d)
  return(summary(model)$adj.r.squared)
}

# Step 3: bootstrapping for 1000 sampling times
set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_AdjR2, R = 1000)

# view results
boot_results 
plot(boot_results)

# get 95% confidence interval
boot.ci(boot_results, type = "perc")


# ----Coefficient----

boot_coef <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE +
                wdayindex + solar_S + wind + DSN + I(DSN^2), data = d)
  return(coef(model))
}
# Step 3: bootstrapping
set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_coef, R = 1000)

# create a data frame for storage
ci_table <- data.frame(
  Coefficient = names(coef(model_with_lag)),
  Estimate = as.numeric(coef(model_with_lag)),
  Lower_95 = NA,
  Upper_95 = NA
)

# bootstrap confidence interval
for (i in 1:nrow(ci_table)) {
  ci <- boot.ci(boot_results, type = "perc", index = i)
  if (!is.null(ci$percent)) {
    ci_table$Lower_95[i] <- ci$percent[4]
    ci_table$Upper_95[i] <- ci$percent[5]
  }
}
ci_table$Significant <- ifelse(ci_table$Lower_95 > 0 | ci_table$Upper_95 < 0, "-", "No")
print(ci_table)

# Print for latex format
#library(xtable)
#xtable(ci_table)


# ----Q3----

# ----Q4----

# Mapping: 1991–2000 → 2014–2023
mapping <- setNames(2014:2023, as.character(2004:2013))

# Extract data from 1991–2000 for prediction
demand_df_future <- demand_df %>%
  filter(start_year %in% names(mapping)) %>%
  mutate(simulated_year = mapping[as.character(start_year)])  # Keep original start_year for prediction

# Predict using the model (start_year unchanged, model runs without error)
demand_df_future$predicted_demand <- predict(model_with_lag, newdata = demand_df_future)

# simulated_year is the time label you want to analyze for the future
summary_stats_sim <- demand_df_future %>%
  group_by(simulated_year) %>%
  summarise(
    max_predicted = max(predicted_demand, na.rm = TRUE)
  )

# Step 1: Actual demand data
actual_data <- tibble(
  simulated_year = 2014:2023,
  actual_demand = c(53200, 52300, 51600, 50700, 48800,
                    46800, 47400, 47100, 47100, 45800)
)

# Step 3: Merge the two data frames
combined_data <- summary_stats_sim %>%
  left_join(actual_data, by = "simulated_year") %>%
  mutate(
    max_predicted = signif(max_predicted, 3),
    actual_demand = signif(actual_demand, 3)
  )

# Step 4: Convert to long format for plotting
plot_data <- combined_data %>%
  pivot_longer(cols = c(max_predicted, actual_demand),
               names_to = "type",
               values_to = "demand")

# Step 5: Plot line chart
ggplot(plot_data, aes(x = simulated_year, y = demand, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Predicted vs Actual Demand",
       x = "Year",
       y = "Demand",
       color = "Type") +
  theme_minimal()

# Create comparison table with error metrics
comparison_table <- combined_data %>%
  mutate(
    abs_diff = abs(max_predicted - actual_demand),
    rel_diff = abs_diff / actual_demand * 100, 
    within_10pct = ifelse(rel_diff <= 10, TRUE, FALSE), 
    within_5pct = ifelse(rel_diff <= 5, TRUE, FALSE)
  )



# ----linear model fit temp solar wind----

# ---- Estimate and project weather variable trends (temp, wind, solar_S) ----

# Step 1: Calculate annual average weather values from 1991 to 2000
historical_weather <- demand_df %>%
  filter(year >= 1991 & year <= 2000) %>%
  group_by(year) %>%
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    avg_wind = mean(wind, na.rm = TRUE),
    avg_solar = mean(solar_S, na.rm = TRUE)
  )

# Step 2: Fit linear trends for each variable
temp_trend <- lm(TE ~ year, data = demand_df)
wind_trend <- lm(avg_wind ~ year, data = historical_weather)
solar_trend <- lm(avg_solar ~ year, data = historical_weather)

# Step 3: Extract slopes (annual change)
temp_slope <- coef(temp_trend)[["year"]]
wind_slope <- coef(wind_trend)[["year"]]
solar_slope <- coef(solar_trend)[["year"]]

# Step 4: Get the baseline (year 2000) values
base_temp <- historical_weather %>% filter(year == 2000) %>% pull(avg_temp)
base_wind <- historical_weather %>% filter(year == 2000) %>% pull(avg_wind)
base_solar <- historical_weather %>% filter(year == 2000) %>% pull(avg_solar)

# Step 5: Project average values from 2014 to 2023
future_years <- 2014:2023
years_since_2000 <- future_years - 2000

future_weather <- data.frame(
  simulated_year = future_years,
  simulated_temp = base_temp + temp_slope * years_since_2000,
  simulated_wind = base_wind + wind_slope * years_since_2000,
  simulated_solar = base_solar + solar_slope * years_since_2000
)

# Step 6: Inject future climate values into prediction dataset
demand_df_future <- demand_df_future %>%
  left_join(future_weather, by = "simulated_year") %>%
  mutate(
    temp = simulated_temp,
    wind = simulated_wind,
    solar_S = simulated_solar
  )

# Step 7: Generate updated predictions using the adjusted weather values
demand_df_future$predicted_demand_adjusted <- predict(model_with_lag, newdata = demand_df_future)


# ----mean of TE, solar_S, wind----

yearly_avg <- demand_df %>%
  group_by(start_year) %>%
  summarise(
    avg_TE = mean(TE, na.rm = TRUE),
    avg_solar_S = mean(solar_S, na.rm = TRUE),
    avg_wind = mean(wind, na.rm = TRUE)
  )

yearly_avg$start_year <- as.numeric(as.character(yearly_avg$start_year))

# 折线图：TE
ggplot(yearly_avg, aes(x = start_year, y = avg_TE)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  geom_point(color = "#1f77b4", size = 2) +
  labs(title = "Yearly Average of TE",
       x = "Year",
       y = "Average TE") +
  theme_minimal(base_size = 14)

# 折线图：solar_S
ggplot(yearly_avg, aes(x = start_year, y = avg_solar_S)) +
  geom_line(color = "#ff7f0e", size = 1.2) +
  geom_point(color = "#ff7f0e", size = 2) +
  labs(title = "Yearly Average of solar_S",
       x = "Year",
       y = "Average solar_S") +
  theme_minimal(base_size = 14)

# 折线图：wind
ggplot(yearly_avg, aes(x = start_year, y = avg_wind)) +
  geom_line(color = "#2ca02c", size = 1.2) +
  geom_point(color = "#2ca02c", size = 2) +
  labs(title = "Yearly Average of Wind",
       x = "Year",
       y = "Average Wind") +
  theme_minimal(base_size = 14)









