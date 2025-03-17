### Question 1---

# Define K-fold cross-validation control (10-fold)
set.seed(123)
cv_control <- trainControl(method = "cv", number = 10)

# Train models using K-fold CV
cv_model_temp <- train(demand_gross ~ temp, data = demand_data, method = "lm", trControl = cv_control)
cv_model_TO <- train(demand_gross ~ TO, data = demand_data, method = "lm", trControl = cv_control)
cv_model_TE <- train(demand_gross ~ TE, data = demand_data, method = "lm", trControl = cv_control)
cv_model_inv_temp <- train(demand_gross ~ I(1/temp), data = demand_data, method = "lm", trControl = cv_control)

# Extract RMSE, MSE, MAE from cross-validation results
cv_results <- tibble(
  Model = c("temp", "TO", "TE", "1/temp"),
  RMSE = c(cv_model_temp$results$RMSE,
           cv_model_TO$results$RMSE,
           cv_model_TE$results$RMSE,
           cv_model_inv_temp$results$RMSE),
  MSE = c(cv_model_temp$results$RMSE^2,
          cv_model_TO$results$RMSE^2,
          cv_model_TE$results$RMSE^2,
          cv_model_inv_temp$results$RMSE^2),
  MAE = c(cv_model_temp$results$MAE,
          cv_model_TO$results$MAE,
          cv_model_TE$results$MAE,
          cv_model_inv_temp$results$MAE)
)

# Display results
print(cv_results)

# Find the best model based on lowest RMSE
best_model <- cv_results %>% arrange(RMSE)
print(best_model)



### Question 3----

# Select winter season from November 2013 to March 2014
demand_2013_14 <- demand_data %>%
  filter(start_year %in% c(2013, 2014))

# Find peak demand in 2013-14
max_demand_2013_14 <- max(demand_2013_14$demand_gross, na.rm = TRUE)
print(paste("Max Demand in 2013-14:", max_demand_2013_14, "MW"))

# Get historical winter temperatures, wind speed, and solar energy
demand_past_years <- demand_data %>%
  filter(start_year %in% c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999)) %>%
  select(TE, wind, solar_S, wdayindex, monthindex, start_year)

# Select winter data from 199X (e.g., 1991-92)
simulated_weather <- demand_past_years %>%
  filter(start_year == 1991) %>%  # You can change this to other years
  mutate(start_year = 2013)  # Replace start_year to 2013

# Predict demand using final_model
simulated_weather$predicted_demand <- predict(final_model, newdata = simulated_weather)

# Find new peak demand under simulated weather conditions
simulated_max_demand <- simulated_weather %>%
  summarise(max_predicted_demand = max(predicted_demand, na.rm = TRUE))

print(simulated_max_demand)
