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


AIC(step_model)
AIC(full_model_1)

par(mfrow=c(2,2))
plot(step_model)
par(mfrow=c(2,2))
plot(full_model_1)






### Q3

final_model <- lm(
  demand_gross ~ start_year + start_year:TE +
    wdayindex + solar_S + wind
  + DSN + I(DSN^2),
  data = demand_df
)


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
       legend = c("Counterfactual/New Max", "Actual 2013–14", "Model Baseline 2013–14"),
       col    = c("black", "red", "blue"),
       lty    = c(1, 2, 3),
       pch    = c(16, NA, NA),
       lwd    = 2)

### Note that the x-axes is the year which data is replace into the model 
### Each point are all predicted 2013-2014, but got replaced by different year in the model





