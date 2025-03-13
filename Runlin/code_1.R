
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

# Compare AIC or R-squared
AIC(model_temp, model_TO, model_TE)



# Task 2

