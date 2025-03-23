# Assume your data frame is called demand_df 
# and has columns:
#   demand_gross    (dependent variable)
#   start_year      (integer winter label, e.g. 1990, 1991, etc.)
#   TE              (the TE_t variable)
#   DOW             (day-of-week indicator, e.g. 0=Sunday,...,6=Saturday)
#   DSN             (days since 1 November)
# Make sure 'start_year' and 'DOW' are set as factors.

library(readr)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(lubridate)


demand_df <- read.csv("SCS_demand_modelling.csv", 
                      stringsAsFactors = FALSE)


demand_df$Date <- as.Date(demand_df$Date, format = "%Y-%m-%d")
# 1) Define a function that checks if a row is in the "Christmas week"
is_christmas_week <- function(d) {
  # Convert each date to a "month-day" format, e.g. "12-24"
  md <- format(d, "%m-%d")
  # Return TRUE if between 12-24 and 12-26
  (md >= "12-23" & md <= "12-27")
}

# 2) Subset to KEEP rows that are NOT in that range
demand_df_cleaned <- subset(demand_df, !is_christmas_week(Date))
demand_df <- demand_df_cleaned 

demand_df_cleaned <- subset(demand_df, format(Date, "%m-%d") != "01-01")
# 3) Check results
nrow(demand_df_cleaned)  # how many rows remain




#demand_df <- read.csv("SCS_demand_modelling.csv", stringsAsFactors = FALSE)
demand_df <- demand_df_cleaned 

############################################
# 1) Prepare the dataset
############################################
# Suppose your main dataset is called `demand_df`.
# It has columns: 
#   demand_gross, start_year, TE, wdayindex, DSN, etc.
# 
# Convert `start_year` to factor so each winter becomes a dummy,
# using 1990 as the baseline (reference) winter.
demand_df$start_year <- factor(demand_df$start_year)
demand_df$start_year <- relevel(demand_df$start_year, ref = "1991")

# Convert `wdayindex` to factor if you want day-of-week categories.
# We can set Sunday (0) as the reference.
demand_df$wdayindex <- factor(demand_df$wdayindex,
                              levels = c("0","1","2","3","4","5","6"))
demand_df$wdayindex <- relevel(demand_df$wdayindex, ref = "0")

############################################
# 2) Fit the model
############################################
# This formula structure encodes:
#   • An intercept + winter dummies (start_year)
#   • year-specific slopes for TE (start_year:TE)
#   • day-of-week categories (wdayindex)
#   • DSN and DSN^2 for a quadratic effect in "days since Nov 1"

model_1 <- lm(
  demand_gross ~ start_year + start_year:TE +
    wdayindex + DSN + I(DSN^2),
  data = demand_df
)

summary(model_1)

model_2 <- lm(
  demand_gross ~ start_year + start_year:TE + solar_S + wind +
    wdayindex + DSN + I(DSN^2),
  data = demand_df
)

summary(model_2)



########################################
# 1) Convert your Date column to Date class
########################################
# (Assuming your dataset is demand_df and 
#  Date is in "YYYY-MM-DD" format.)
#demand_df$Date <- as.Date(demand_df$Date, format = "%Y-%m-%d")

########################################
# 2) Create a subset for 2014–15
#    Typically, 'start_year == 2014' 
#    covers 1 Nov 2014 to 31 Mar 2015
########################################
df_1314 <- subset(demand_df, start_year == 2013)

########################################
# 3) Plot side-by-side or stacked
#    We'll do stacked: (2 rows, 1 column)
########################################
par(mfrow = c(2,1), mar = c(4,4,3,1))

# 3a) Top plot: All years
plot(demand_df$Date, demand_df$demand_gross,
     type = "l", col = "blue",
     main = "All years",
     xlab = "Time",
     ylab = "Peak demand (MW)")

plot(demand_df_cleaned$Date, demand_df_cleaned$demand_gross,
     type = "l", col = "blue",
     main = "All years without Chrismas",
     xlab = "Time",
     ylab = "Peak demand (MW)")

# 3b) Bottom plot: 2014–15 only
plot(df_1314$Date, df_1314$demand_gross,
     type = "l", col = "blue",
     main = "2013/14",
     xlab = "Time",
     ylab = "Peak demand (MW)")


full_model_1 <- lm(
  demand_gross ~ start_year + start_year:TE +
    wdayindex + solar_S + wind
    + DSN + I(DSN^2),
  data = demand_df
)

summary(full_model_1)



full_model_2 <- lm(
  demand_gross ~ start_year + start_year:TE + TE + I(TE^2)+
    wdayindex + solar_S + wind + DSN + I(DSN^2) +poly(wdayindex,2),
  data = demand_df
)
summary(full_model_2)
acf(full_model_1$residuals, main="Autocorrelation")



# 假设你的数据框是 demand_df，里面有 demand_gross, TE 等变量
full_model_3 <- lm(
  demand_gross ~ start_year + wdayindex + wind + solar_S +
    DSN + I(DSN^2) + TE + I(TE^2), 
  data = demand_df
)

summary(full_model_3)


summary(full_model_2)


AIC(full_model_1)
AIC(full_model_2)


acf(full_model_2$residuals, main="Autocorrelation")

backup_full <- lm(
  demand_gross ~ start_year*TE + wdayindex + start_year*wind*solar_S +
    DSN + I(DSN^2) + TE + I(TE^2) + TE*wind*solar_S,  
  data = demand_df
)


AIC(full_model_1)

par(mfrow=c(2,2))
plot(step_model)
par(mfrow=c(2,2))
plot(full_model_1)






### Q3
par(mfrow=c(1,1))
final_model <- lm(
  demand_gross ~ start_year + start_year:TE +
    wdayindex + solar_S + wind
  + DSN + I(DSN^2),
  data = demand_df
)

demand_modelling_filtered
demand_df <- demand_modelling_filtered  %>%
  filter(!(format(Date, "%m-%d") %in% c("01-01", "12-23", "12-24", "12-25", "12-26", "12-27")))


demand_df <- demand_df %>%
  arrange(Date) %>%
  mutate(lag1_demand = lag(demand_gross, 1))

final_model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                       solar_S + wind + DSN + I(DSN^2), data = demand_df)

summary(final_model)

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
       legend = c("Counterfactual/New Max", "Actual 2013–14", "Model Baseline 2013–14"),
       col    = c("black", "red", "blue"),
       lty    = c(1, 2, 3),
       pch    = c(16, NA, NA),
       lwd    = 2)

### Note that the x-axes is the year which data is replace into the model 
### Each point are all predicted 2013-2014, but got replaced by different year in the model







###question 3 (way2)



final_model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                    solar_S + wind + DSN + I(DSN^2), data = demand_df)
summary(final_model)

# Suppose your complete dataset is `demand_df`
# which spans multiple winters 1991..2015, or similar.
# The column 'start_year' indicates which winter each row belongs to.

train_data <- subset(demand_df, start_year != 2013)  # Exclude 2013
test_2013  <- subset(demand_df, start_year == 2013)  # only 2013

# Example formula; adapt to your actual variables
model_no2013 <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                     solar_S + wind + DSN + I(DSN^2),
  data = train_data
)

summary(model_no2013)

# Predict using the new model

pred_2013 <- predict(model_no2013, newdata = test_2013)

# The "model-based" maximum for 2013–14
model_based_max_2013 <- max(pred_2013, na.rm = TRUE)

# The actual observed max in 2013–14
actual_max_2013 <- max(test_2013$demand_gross, na.rm = TRUE)

cat("Model-based 2013–14 Max (trained w/o 2013):", model_based_max_2013, "\n")
cat("Actual 2013–14 Max Demand:", actual_max_2013, "\n")

compute_max_demand_for_hist_weather <- function(model, baseline_2013, hist_winter) {
  # Make a copy of 2013–14 skeleton
  scenario_df <- baseline_2013
  
  # Merge or replace weather columns from 'hist_winter'.
  # Typically, you'd align them by DSN or day index:
  scenario_df <- merge(
    scenario_df[ , !(names(scenario_df) %in% c("temp","wind","solar_S","TE","lag1_demand"))],
    hist_winter[ , c("DSN","temp","wind","solar_S","TE","lag1_demand")],
    by = "DSN",
    all.x = TRUE
  )
  
  # Predict demand
  pred <- predict(model, newdata = scenario_df)
  max(pred, na.rm = TRUE)
}

# Identify all winters except 2013
unique_winters <- sort(unique(demand_df$start_year))
unique_winters <- unique_winters[unique_winters != 2013]

results <- data.frame(
  hist_winter_start = integer(),
  max_pred_demand   = numeric(),
  stringsAsFactors = FALSE
)

# We'll call the test_2013 as 'baseline_2013'
for(yr in unique_winters) {
  hist_winter_df <- subset(demand_df, start_year == yr)
  
  scenario_max <- compute_max_demand_for_hist_weather(
    model          = model_no2013,
    baseline_2013  = test_2013,
    hist_winter    = hist_winter_df
  )
  
  results <- rbind(results, 
                   data.frame(hist_winter_start = yr,
                              max_pred_demand = scenario_max))
}

# Inspect
results


# Assume 'results' has:
#   results$hist_winter_start       (the older winter, e.g. 1991, 1992...)
#   results$max_pred_demand        (predicted max for that scenario)
# And you have numeric scalars:
#   actual_max_2013
#   model_based_max_2013

# Basic bar chart
barplot(
  height    = results$max_pred_demand,
  names.arg = results$hist_winter_start,
  main      = "2013–14 Peak Demand Under Older Winters' Weather",
  xlab      = "Historic Winter (start year)",
  ylab      = "Model-Predicted Max Demand (MW)",
  las       = 2            # rotate labels if needed
)

# Add horizontal lines for actual and model-based 2013 peaks
abline(h = actual_max_2013, col = "red", lwd = 2, lty = 2)
abline(h = model_based_max_2013, col = "blue", lwd = 2, lty = 3)

legend(
  "topright",
  legend = c("Actual 2013–14 Peak", "Model-Based 2013–14"),
  col    = c("red", "blue"),
  lty    = c(2, 3),
  lwd    = 2
)

# 1) Sort results by hist_winter_start (if not already sorted)
results_sorted <- results[order(results$hist_winter_start), ]

plot(
  x    = results_sorted$hist_winter_start,
  y    = results_sorted$max_pred_demand,
  type = "o",
  pch  = 16,
  lty  = 1,
  lwd  = 2,
  ylim = range(results_sorted$max_pred_demand, actual_max_2013, model_based_max_2013),
  xlab = "Historic Winter (start year)",
  ylab = "Model-Predicted Max Demand (MW)",
  main = "2013–14 Peak Demand Under Older Winters' Weather"
)
abline(h = actual_max_2013, col = "red", lty = 2, lwd = 2)
abline(h = model_based_max_2013, col = "blue", lty = 3, lwd = 2)

legend(
  "topleft",
  legend = c("New Peak after substitution", "Actual 2013–14 Max", "Model-predited 2013–14 Max"),
  col    = c("black", "red", "blue"),
  lty    = c(1, 2, 3),
  pch    = c(16, NA, NA),
  lwd    = 2
)






###q3, redo

##############################
# 1. Prepare the Training Data 
# (exclude 2013 from the training set)
##############################
train_data <- subset(demand_df, start_year != 2013)

# Ensure 'start_year' factor retains all possible levels, including "2013"
train_data$start_year <- factor(
  train_data$start_year,
  levels = levels(demand_df$start_year)
)

##############################
# 2. Fit the Model on Data Excluding 2013
##############################
# This model includes an intercept shift by winter, a winter-specific temperature slope,
# day-of-week effects, wind, solar, a quadratic in DSN, and lagged demand.
model_no2013 <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                     solar_S + wind + DSN + I(DSN^2),
  data = train_data
)
summary(model_no2013)

##############################
# 3. Create the 2013 Baseline (Skeleton)
# We use the 2013 data for its calendar structure (dates, DSN, wdayindex, etc.)
##############################
# The 2013 "skeleton" means all columns for 2013
# We'll soon overwrite the weather columns with older winters' values.
baseline_2013 <- subset(demand_df, start_year == 2013)

# Sort them by DSN if needed (optional)
baseline_2013 <- baseline_2013[order(baseline_2013$DSN), ]


compute_scenario_max <- function(model, baseline_2013, hist_winter_df) {
  # 1) Sort the older winter data to match row order
  hist_winter_df <- hist_winter_df[order(hist_winter_df$DSN), ]
  
  # 2) Make a copy of the baseline 2013
  scenario_df <- baseline_2013
  
  # 3) Overwrite the weather columns from hist_winter_df
  #    (and anything else you want to "import" from the older winter)
  scenario_df$TE         <- hist_winter_df$TE
  scenario_df$wind       <- hist_winter_df$wind
  scenario_df$solar_S    <- hist_winter_df$solar_S
  scenario_df$lag1_demand <- hist_winter_df$lag1_demand
  
  # 4) Predict daily demand using the model (trained w/o 2013)
  preds <- predict(model, newdata = scenario_df)
  
  # 5) Return the maximum predicted demand
  max(preds, na.rm = TRUE)
}


# 1) Identify older winters in 'train_data'
unique_winters <- sort(unique(train_data$start_year))

# 2) Prepare a results data frame
results <- data.frame(
  hist_year = unique_winters,
  max_pred  = NA_real_
)

# 3) For each older winter, substitute its weather
for (i in seq_along(unique_winters)) {
  yr <- unique_winters[i]
  hist_winter_df <- subset(demand_df, start_year == yr)
  
  # Possibly ensure hist_winter_df has the same # of rows as baseline_2013
  # and is in the same DSN order (as we do above)
  
  results$max_pred[i] <- compute_scenario_max(
    model_no2013,
    baseline_2013,
    hist_winter_df
  )
}

results


plot(
  x    = results$hist_year,
  y    = results$max_pred,
  type = "o",
  pch  = 16, lty = 1, lwd = 2,
  xlab = "Historic Winter (start year)",
  ylab = "Counterfactual Max Demand (MW)",
  main = "2013–14 Peak Demand Under Older Winters' Weather"
)

# (Optional) Add reference lines for actual 2013 max or model-based 2013 max
# from the real 2013 weather
actual_2013 <- max(baseline_2013$demand_gross, na.rm = TRUE)
abline(h = actual_2013, col = "red", lwd = 2, lty = 2)
