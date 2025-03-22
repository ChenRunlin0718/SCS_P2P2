
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
  return(summary(model)$r.square)
}

# Step 3: 执行 bootstrapping
set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_fn, R = 1000)

# Step 4: 查看置信区间
# 创建空 data frame 存结果
param_names <- names(coef(lm(demand_gross ~ lag1_demand + start_year + start_year:TE +
                               wdayindex + solar_S + wind + DSN + I(DSN^2), data = demand_df)))

ci_table <- data.frame(Parameter = param_names,
                       Estimate = NA,
                       CI_Lower = NA,
                       CI_Upper = NA)

# 填充表格
for (i in 1:length(param_names)) {
  ci <- boot.ci(boot_results, type = "perc", index = i)
  ci_table$Estimate[i] <- boot_results$t0[i]
  ci_table$CI_Lower[i] <- ci$percent[4]  # 4th value = 2.5% percentile
  ci_table$CI_Upper[i] <- ci$percent[5]  # 5th value = 97.5% percentile
}







# function to obtain R-Squared from the data
rsq <- function(formula, data, indices)
{
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")
