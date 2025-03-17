
demand_df <- read.csv("SCS_demand_modelling.csv", 
                      stringsAsFactors = FALSE)

## Task 1
# How does peak daily demand depend on temperature?

# Inspect the first few rows
head(demand_df)

plot(demand_df$temp, demand_df$demand_gross,
     xlab = "Temperature (°C)",
     ylab = "Peak Daily Demand (MW)",
     main = "Peak Daily Demand vs. Temperature at 6pm")

model_temp <- lm(demand_gross ~ temp, data = demand_df)
summary(model_temp)

# Diagnostic plots (check assumptions, look for outliers)
par(mfrow = c(2,2))
plot(model_temp)

# Reset plotting layout
par(mfrow = c(1,1))

#Compare different temperature definitions
#    (temp, TO, and TE) if desired
model_TO <- lm(demand_gross ~ TO, data = demand_df)
model_TE <- lm(demand_gross ~ TE, data = demand_df)

summary(model_TO)
summary(model_TE)

# Method 1: Compare AIC or R-squared
AIC(model_temp, model_TO, model_TE)
BIC(model_temp, model_TO, model_TE)
summary(model_temp)$adj.r.squared
summary(model_TO)$adj.r.squared
summary(model_TE)$adj.r.squared

# Method 2: Bootstrap method


original_model <- lm(demand_gross ~ temp, data = demand_df)

# 1) Set up bootstrap parameters
set.seed(123)     # for reproducibility
B <- 1000         # number of bootstrap replications
n <- nrow(demand_df)

# 2) Create storage vectors for each bootstrap replicate
boot_slope  <- numeric(B)
boot_int    <- numeric(B)

# 3) Loop over bootstrap samples
for(i in seq_len(B)) {
  # a) draw a sample of row indices, with replacement
  idx <- sample(seq_len(n), size = n, replace = TRUE)
  
  # b) subset the data
  boot_data <- demand_df[idx, ]
  
  # c) fit the model on this bootstrap sample
  fit <- lm(demand_gross ~ temp, data = boot_data)
  
  # d) store the coefficients
  boot_slope[i] <- coef(fit)["temp"]
  boot_int[i]   <- coef(fit)["(Intercept)"]
}

# Original slope
original_slope <- coef(original_model)["temp"]
cat("Original slope estimate:", original_slope, "\n")

# Bootstrap mean slope
cat("Bootstrap mean slope:", mean(boot_slope), "\n")

# 95% percentile-based confidence interval
ci_slope <- quantile(boot_slope, probs = c(0.025, 0.975))
cat("95% CI for the slope from bootstrap:", ci_slope, "\n")

# -------------------------------------------------
# Optional: Plot the bootstrap distribution
# -------------------------------------------------
hist(boot_slope, breaks = 30,
     main = "Bootstrap distribution of slope (demand vs. temp)",
     xlab = "Slope coefficient for temperature")

abline(v = original_slope, col = "red", lwd = 2)
abline(v = ci_slope, col = "blue", lty = 2, lwd = 2)
legend("topright", legend = c("Original slope","95% CI"),
       col = c("red","blue"), lty = c(1,2), lwd = 2)




### Find the best model (unfinished)
model_1 = lm(demand_gross ~ TE,
             data = demand_df)
model_2 = lm(demand_gross ~ TE + wind,
             data = demand_df)
model_3 = lm(demand_gross ~ TE + wind + wdayindex,
             data = demand_df)

AIC(model_1, model_2, model_3)
summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared
summary(model_3)$adj.r.squared





# Task 2
# How well does the model fit the historic data?
final_model <- lm(demand_gross ~ temp + wind + wdayindex,
                  data = demand_df)

## Method 1: Check the assumptions
# 这里假设最好的model 是TE，因为他AIC最低和R^2最高
par(mfrow=c(2,2))
plot(final_model)  # Creates 4 common diagnostic plots
par(mfrow=c(1,1))


# Task 3
# How could the maximum annual demand have varied in the 2013-14 winter season if different weather conditions had occurred? 

#########################################
# (1) Fit the final model
#########################################
final_model <- lm(demand_gross ~ temp + wind + wdayindex,
                  data = demand_df)

#########################################
# (2) Subset the 2013–14 winter data
#########################################
winter_1314 <- subset(demand_df, start_year == 2013)

# The actual max demand in 2013–14:
actual_max_1314 <- max(winter_1314$demand_gross, na.rm = TRUE)

# Also predict baseline 2013–14 demand with its own actual weather,
# for reference, so we have a "model-based" 2013–14 max
predicted_1314 <- predict(final_model, newdata = winter_1314)
model_based_max_1314 <- max(predicted_1314, na.rm = TRUE)

cat("Actual 2013–14 max demand:", actual_max_1314, "\n")
cat("Model-based 2013–14 max demand:", model_based_max_1314, "\n")

#########################################
# (3) Helper: substitute weather, get predicted max
#########################################
compute_max_demand_for_historic_weather <- function(model,
                                                    baseline_1314,
                                                    hist_winter) {
  # 1. Copy the 2013–14 dataset
  scenario_df <- baseline_1314
  
  # 2. Merge the older winter’s weather columns into 2013–14 rows
  #    by DSN so day i lines up with day i
  scenario_df <- merge(
    scenario_df[ , !(names(scenario_df) %in% c("temp", "wind"))],
    hist_winter[ , c("DSN", "temp", "wind")],
    by = "DSN",
    all.x = TRUE
  )
  
  # 3. Predict demand using the final model
  pred <- predict(model, newdata = scenario_df)
  
  # 4. Return the max predicted demand
  max(pred, na.rm = TRUE)
}

#########################################
# (4) Loop over each older winter
#########################################
unique_winters <- sort(unique(demand_df$start_year))
results <- data.frame(
  winter_start          = integer(),
  max_pred_demand       = numeric(),
  diff_from_actual_1314 = numeric(), # compare to actual 2013–14
  diff_from_model_1314  = numeric()  # compare to predicted 2013–14
)

for(yr in unique_winters) {
  if(yr == 2013) next  # skip or not, up to you
  
  hist_winter_df <- subset(demand_df, start_year == yr)
  
  # Make sure lengths match or handle partial merges. 
  # Then compute the scenario's max
  scenario_max <- compute_max_demand_for_historic_weather(
    model          = final_model,
    baseline_1314  = winter_1314,
    hist_winter    = hist_winter_df
  )
  
  # Compare to the *actual* 2013–14 max
  delta_actual <- scenario_max - actual_max_1314
  
  # Compare to the *model-based* 2013–14 max
  delta_model  <- scenario_max - model_based_max_1314
  
  results <- rbind(results, data.frame(
    winter_start          = yr,
    max_pred_demand       = scenario_max,
    diff_from_actual_1314 = delta_actual,
    diff_from_model_1314  = delta_model
  ))
}

#########################################
# (5) Look at results
#########################################
print(results)

# Make sure results$winter_start is sorted in ascending order
results <- results[order(results$winter_start), ]

plot(
  x    = results$winter_start,
  y    = results$max_pred_demand,
  type = "o",           # 'o' means draw both lines and points
  pch  = 16,            # plotting symbol
  lty  = 1,             # solid line
  lwd  = 2,             # line thickness
  xlab = "Historic Winter (start year)",
  ylab = "Max Demand (MW)",
  main = "Counterfactual 2013–14 Max Demand vs. Actual & Model Baseline"
)

# Add a horizontal line for the *actual* 2013–14 maximum
abline(h = actual_max_1314, col = "red", lwd = 2, lty = 2)

# Add a horizontal line for the *model-based* 2013–14 maximum
abline(h = model_based_max_1314, col = "blue", lwd = 2, lty = 3)

# Legend
legend("topright",
       legend = c("Counterfactual Max", "Actual 2013–14", "Model Baseline 2013–14"),
       col    = c("black", "red", "blue"),
       lty    = c(1, 2, 3),
       pch    = c(16, NA, NA),
       lwd    = 2)

### Note that the x-axes is the year which data is replace into the model 
### Each point are all predicted 2013-2014, but got replaced by different year in the model


