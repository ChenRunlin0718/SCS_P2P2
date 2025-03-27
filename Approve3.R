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
final_model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                       solar_S + wind + DSN + I(DSN^2), data = demand_df)
summary(final_model)


par(mfrow=c(1,1))

#########################################
# (2) Subset the 2013–14 winter data
#########################################
winter_1314 <- subset(demand_df, start_year == 2013)
winter_without1314 <- subset(demand_df, start_year != 2013)
# The actual max demand in 2013–14:
actual_max_1314 <- max(winter_1314$demand_gross, na.rm = TRUE)

# Also predict baseline 2013–14 demand with its own actual weather,
# for reference, so we have a "model-based" 2013–14 max
predicted_1314 <- predict(final_model, newdata = winter_1314)   ####### winter_1314 by Chatgpt
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
    scenario_df[ , !(names(scenario_df) %in% c("TE", "wind","solar_S"))],
    hist_winter[ , c("DSN", "TE", "wind","solar_S")],
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
    baseline_1314  = winter_1314,  ### winter_1314 by AI
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



### With blue line (model_predicted max)
plot(
  x    = results$winter_start,
  y    = results$max_pred_demand,
  type = "o",
  pch  = 16, lty = 1, lwd = 2,
  ylim = range(results$max_pred_demand, 
               actual_max_1314, 
               model_based_max_1314),
  xlab = "Historic Winter (start year)",
  ylab = "Max Demand (MW)",
  main = "Counterfactual 2013–14 Max Demand vs. Actual & Model Baseline"
)
abline(h = actual_max_1314, col = "red", lwd = 2, lty = 2)
abline(h = model_based_max_1314, col = "blue", lwd = 2, lty = 3)

# Legend
legend("center",
       legend = c("New Peak after substitution", "Actual 2013–14 Max", "Model-predited 2013–14 Max"),
       col    = c("black", "red", "blue"),
       lty    = c(1, 2, 3),
       pch    = c(16, NA, NA),
       lwd    = 2)


### Without blue line (model_predicted max)
par(mfrow=c(1,1))
plot(
  x    = results$winter_start,
  y    = results$max_pred_demand,
  type = "o",              # 'o' means draw both lines and points
  pch  = 16,               # plotting symbol
  lty  = 1,                # solid line
  lwd  = 2,                # line thickness
  ylim = range(results$max_pred_demand, actual_max_1314),
  xlab = "Historic Winter (start year)",
  ylab = "Max Demand (MW)",
  main = "Counterfactual 2013–14 Max Demand vs. Actual"
)

# Add a horizontal line for the *actual* 2013–14 maximum
abline(h = actual_max_1314, col = "red", lwd = 2, lty = 2)

# Legend (without blue line)
legend("center",
       legend = c("New Peak after substitution", "Actual 2013–14 Max"),
       col    = c("black", "red"),
       lty    = c(1, 2),
       pch    = c(16, NA),
       lwd    = 2
)




### Q3 with bootstrap


# ----Q4----




