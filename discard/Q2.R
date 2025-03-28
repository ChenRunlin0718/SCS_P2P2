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
summary(model_with_lag)$sigma^2
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
  filter(!is.na(lag1_demand))  # 移除第一行 NA

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








