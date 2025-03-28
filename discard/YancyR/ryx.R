### Settings----

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(broom)
library(tibble)

# Load datasets
demand_data <- read.csv("SCS_demand_modelling.csv")
temp_data <- read.csv("SCS_hourly_temp.csv")

# Convert date column
demand_data$Date <- as.Date(demand_data$Date, format="%Y-%m-%d")




### Question 1----

ggplot(demand_data, aes(x = temp, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  labs(title = "Peak Demand vs. Temperature",
       x = "Temperature (°C)", y = "Peak Demand (MW)") +
  theme_minimal()

# Compute correlation
cor(demand_data$temp, demand_data$demand_gross, use="complete.obs")

# Build regression models
model_temp <- lm(demand_gross ~ temp, data = demand_data)
model_TO <- lm(demand_gross ~ TO, data = demand_data)
model_TE <- lm(demand_gross ~ TE, data = demand_data)
model_temp1 <- lm(demand_gross ~ I(1/temp), data = demand_data)

# residual plots
par(mfrow = c(2, 2))
plot(model_temp, main = "Residual Plots for model_temp")
plot(model_TO, main = "Residual Plots for model_TO")
plot(model_TE, main = "Residual Plots for model_TE")
plot(model_temp1, main = "Residual Plots for model_temp1")

# Compare model performance
summary(model_temp)
summary(model_TO)
summary(model_TE)
summary(model_temp1)



## Extract R-squared values
r_squared_values <- data.frame(
  Model = c("model_temp", "model_TO", "model_TE", "model_temp1"),
  R_squared = c(summary(model_temp)$r.squared, 
                summary(model_TO)$r.squared, 
                summary(model_TE)$r.squared, 
                summary(model_temp1)$r.squared)
)

# Sort by R-squared in descending order
r_squared_values <- r_squared_values[order(-r_squared_values$R_squared), ]

# Print sorted R-squared values
print(r_squared_values)


## model selection using AIC
aic_values <- AIC(model_temp, model_TO, model_TE, model_temp1)

# Sort models by AIC in ascending order (lower AIC is better)
aic_sorted <- aic_values[order(aic_values$AIC), ]

# Print the sorted AIC values
print(aic_sorted)


## MSE, RMSE, MAE
# Define function to calculate MSE, RMSE, and MAE
calculate_metrics <- function(model, data) {
  predictions <- predict(model, newdata = data)
  residuals <- demand_data$demand_gross - predictions
  
  mse <- mean(residuals^2, na.rm = TRUE)     # Mean Squared Error
  rmse <- sqrt(mse)                          # Root Mean Squared Error
  mae <- mean(abs(residuals), na.rm = TRUE)  # Mean Absolute Error
  
  return(c(MSE = mse, RMSE = rmse, MAE = mae))
}

# Compute metrics for each model
metrics_values <- tibble(
  Model = c("temp", "TO", "TE", "1/temp"),
  MSE = c(
    calculate_metrics(model_temp, demand_data)["MSE"],
    calculate_metrics(model_TO, demand_data)["MSE"],
    calculate_metrics(model_TE, demand_data)["MSE"],
    calculate_metrics(model_temp1, demand_data)["MSE"]
  ),
  RMSE = c(
    calculate_metrics(model_temp, demand_data)["RMSE"],
    calculate_metrics(model_TO, demand_data)["RMSE"],
    calculate_metrics(model_TE, demand_data)["RMSE"],
    calculate_metrics(model_temp1, demand_data)["RMSE"]
  ),
  MAE = c(
    calculate_metrics(model_temp, demand_data)["MAE"],
    calculate_metrics(model_TO, demand_data)["MAE"],
    calculate_metrics(model_TE, demand_data)["MAE"],
    calculate_metrics(model_temp1, demand_data)["MAE"]
  )
)

# Display the table with all metrics
print(metrics_values)



### Question 2----

# Define function to compute MSE
calculate_mse <- function(model, data) {
  predictions <- predict(model, newdata = data)
  actuals <- data$demand_gross
  
  mse <- mean((actuals - predictions)^2, na.rm = TRUE)
  
  return(mse)
}

# list all models
model1 <- lm(demand_gross ~ TE, data = demand_data)
model2 <- lm(demand_gross ~ TE + wind + solar_S, data = demand_data)
model3 <- lm(demand_gross ~ TE + wind + solar_S + wdayindex + monthindex, data = demand_data)
final_model <- lm(demand_gross ~ TE + wind + solar_S + wdayindex + monthindex + start_year, data = demand_data)

# Evaluate models using R², AIC, and MSE
model_performance <- tibble(
  Model = c("Baseline", "Weather", "Time-adjusted", "Final Model"),
  R2 = c(summary(model1)$r.squared, summary(model2)$r.squared, summary(model3)$r.squared, summary(final_model)$r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(final_model)),
  MSE = c(calculate_mse(model1, demand_data),
          calculate_mse(model2, demand_data),
          calculate_mse(model3, demand_data),
          calculate_mse(final_model, demand_data))
)

print(model_performance)

# residual plots
par(mfrow = c(2, 2))
plot(final_model, main = "Residual Plots for final_model")



### Question 3----

# Select winter season from November 2013 to March 2014
demand_2013_14 <- demand_data %>%
  filter(start_year %in% c(2013, 2014))

# Find peak demand in 2013-14
max_demand_2013_14 <- max(demand_2013_14$demand_gross, na.rm = TRUE)
print(paste("Max Demand in 2013-14:", max_demand_2013_14, "MW"))

# Get historical winter temperatures, wind speed, and solar energy (for all 199X years)
demand_past_years <- demand_data %>%
  filter(start_year %in% seq(1991, 2013, by = 1)) %>%
  select(Date, TE, wind, solar_S, wdayindex, monthindex, start_year)

# List of years to simulate
years_to_test <- seq(1991, 2013, by = 1)

# Store results
results <- tibble()

# Loop through each year and compute predicted peak demand
for (y in years_to_test) {
  simulated_weather <- demand_past_years %>%
    filter(start_year == y) %>%
    mutate(start_year = 2013)  # Change start_year to 2013
  
  # Predict demand using final_model
  simulated_weather$predicted_demand <- predict(final_model, newdata = simulated_weather)
  
  # Find new peak demand
  simulated_max_demand <- simulated_weather %>%
    summarise(start_year = y, max_predicted_demand = max(predicted_demand, na.rm = TRUE))
  
  # Store results
  results <- bind_rows(results, simulated_max_demand)
}

# # Add actual 2013-14 peak demand to the results
# results <- results %>%
#   bind_rows(tibble(start_year = 2013, max_predicted_demand = max_demand_2013_14))

# Print results
print(results)

ggplot(results, aes(x = start_year, y = max_predicted_demand)) +
  geom_line(color = "blue", size = 1) +  # Ensure line connects correctly
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = max_demand_2013_14, linetype = "dashed", color = "black") +
  labs(title = "Predicted Peak Demand for 2013-14 Under Different Historical Weather Conditions",
       x = "Simulated Weather Year",
       y = "Predicted Peak Demand (MW)") +
  theme_minimal()



