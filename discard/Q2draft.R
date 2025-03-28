
model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                 wdayindex + solar_S + wind + DSN + I(DSN^2),
               data = demand_df)
summary(model_TE)
# Residuals Plot
par(mfrow=c(2,2))
plot(model_TE)

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


library(lmtest)
# Check whether there is autocorrelation
dwtest(model_TE) # Durbin-Watson Test

acf(model_TE$residuals, main="Autocorrelation")


demand_df <- demand_df %>%
  arrange(Date) %>%
  mutate(lag1_demand = lag(demand_gross, 1))

model_with_lag <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                       solar_S + wind + DSN + I(DSN^2), data = demand_df)
summary(model_with_lag)
AIC(model_with_lag)
par(mfrow=c(1,1))
acf(model_with_lag$residuals, main="Autocorrelation")
dwtest(model_with_lag) 
par(mfrow=c(2,2))
plot(model_with_lag)


library(boot)
# Step 1: 创建滞后变量
demand_df <- demand_df %>%
  arrange(Date) %>%
  mutate(lag1_demand = lag(demand_gross, 1)) %>%
  filter(!is.na(lag1_demand))  # 移除第一行 NA

# Step 2: 定义 boot 函数
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE +
                wdayindex + solar_S + wind + DSN + I(DSN^2), data = d)
  return(summary(model)$adj.r.squared)
}

# Step 3: 执行 bootstrapping
set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_fn, R = 1000)

# view results
boot_results 
plot(boot_results)

# get 95% confidence interval
boot.ci(boot_results, type = "perc")


boot_fn <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE +
                wdayindex + solar_S + wind + DSN + I(DSN^2), data = d)
  return(coef(model))
}
# Step 3: 执行 bootstrapping
set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_fn, R = 1000)
boot.ci(boot_results, type = "perc", index = i)
plot(boot_results, index = 2) 

# 创建一个 data frame 存放结果
ci_table <- data.frame(
  Coefficient = names(coef(model_with_lag)),
  Estimate = as.numeric(coef(model_with_lag)),
  Lower_95 = NA,
  Upper_95 = NA
)

# 填入 bootstrap CI
for (i in 1:nrow(ci_table)) {
  ci <- boot.ci(boot_results, type = "perc", index = i)
  if (!is.null(ci$percent)) {
    ci_table$Lower_95[i] <- ci$percent[4]
    ci_table$Upper_95[i] <- ci$percent[5]
  }
}
ci_table$Significant <- ifelse(ci_table$Lower_95 > 0 | ci_table$Upper_95 < 0, "Yes", "No")
print(ci_table)

library(xtable)
xtable(ci_table)
