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

acf(model_TE$residuals, main="Autocorrelation")

# Block Residual Bootstrapping
demand_df$resid <- residuals(model_TE)
demand_df$fitted <- fitted(model_TE)
winter_blocks <- split(demand_df, demand_df$start_year)
n <- nrow(demand_df)


boot_block_resid <- function(data, i) {
  sampled_blocks <- sample(winter_blocks, length(winter_blocks), replace = TRUE)
  df_star <- do.call(rbind, sampled_blocks)
  df_star <- df_star[order(df_star$Date), ]  # 
  
  resid_star_full <- df_star$resid
  
  #
  if (length(resid_star_full) < n) {
    resid_star_full <- rep(resid_star_full, length.out = n)
  }
  resid_star <- resid_star_full[1:n]
  
  y_star <- demand_df$fitted + resid_star
  data$y_star <- y_star
  
  model_star <- lm(y_star ~ start_year + start_year:TE + 
                     wdayindex + solar_S + wind + DSN + I(DSN^2),
                   data = data)
  return(coef(model_star))
}

set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_block_resid, R = 1000)
set.seed(456)
boot_results2 <- boot(data = demand_df, statistic = boot_block_resid, R = 1000)
colMeans(boot_results$t)
colMeans(boot_results2$t)  # Compare different set seed

# Check SD
#apply(boot_results$t, 2, sd)

# Original Coefficient
#coef(model_TE)

# Check 95% CI
boot.ci(boot_results, type = "perc", index = 2)

# Visualised one coefficient
hist(boot_results$t[, 2], main = "start_year", xlab = "Coefficient")
abline(v = coef(model_TE)[2], col = "red", lwd = 2)


# Original OLS Coefficient
ols_coef <- coef(model_TE)

# bootstrap estimate
boot_mean <- colMeans(boot_results$t)
boot_sd <- apply(boot_results$t, 2, sd)

# Coefficient name
coef_names <- names(ols_coef)

# 初始化空表
ci_matrix <- matrix(NA, nrow = length(ols_coef), ncol = 2)

# 计算每个系数的 CI
for (i in seq_along(ols_coef)) {
  ci <- tryCatch(
    boot.ci(boot_results, type = "perc", index = i)$perc[4:5],  # 4:5 是lower, upper
    error = function(e) c(NA, NA)
  )
  ci_matrix[i, ] <- ci
}

# confidence interval data frame
ci_df <- data.frame(
  Lower_95 = ci_matrix[, 1],
  Upper_95 = ci_matrix[, 2]
)
covers <- (ols_coef >= ci_df$Lower_95) & (ols_coef <= ci_df$Upper_95)
result_table <- data.frame(
  #Coefficient = coef_names,
  OLS_Estimate = round(ols_coef, 4),
  #Boot_Mean = round(boot_mean, 4),
  #Boot_SD = round(boot_sd, 4),
  Lower_95 = round(ci_df$Lower_95, 4),
  Upper_95 = round(ci_df$Upper_95, 4),
  CI_Covers_OLS = ifelse(covers, "-", "No")
)
result_table$Significant_95 <- ifelse(
  (result_table$Lower_95 > 0 & result_table$Upper_95 > 0) |
    (result_table$Lower_95 < 0 & result_table$Upper_95 < 0),
  "-", "No"
)
print(result_table)

#----Q3-----
boot_iter <- 10
results <- data.frame(year = integer(), max_predicted_demand = numeric(),
                      lower_95 = numeric(), upper_95 = numeric())

# 提取模型残差和拟合值
resid_original <- residuals(model_TE)
fitted_values <- fitted(model_TE)
n <- length(fitted_values)

# 添加到原始数据中（用于重采样）
demand_df$resid <- resid_original
demand_df$fitted <- fitted_values

# 构建以 start_year 为单位的 winter block
winter_blocks <- split(demand_df, demand_df$start_year)

# 遍历 1991 到 2012 年，替换 2013 年冬天的天气变量
for (yr in 1991:2012) {
  
  # 拿到目标年（2013）的结构和需要预测的变量
  winter_df <- subset(demand_df, start_year == 2013)
  winter_df$month_day <- format(winter_df$Date, "%m-%d")
  
  # 替代天气数据
  weather_year <- subset(demand_df, year == yr)
  weather_year$month_day <- format(weather_year$Date, "%m-%d")
  
  # 替换 TE, solar_S, wind
  merged_df <- merge(winter_df, weather_year[, c("month_day", "TE", "solar_S", "wind")],
                     by = "month_day", all.x = TRUE, suffixes = c("", "_w"))
  
  merged_df$TE <- merged_df$TE_w
  merged_df$solar_S <- merged_df$solar_S_w
  merged_df$wind <- merged_df$wind_w
  merged_df <- merged_df[, !(names(merged_df) %in% c("TE_w", "solar_S_w", "wind_w"))]
  
  # 计算拟合值（fixed），用于加残差
  predicted_fixed <- predict(model_TE, newdata = merged_df)
  
  # 创建 bootstrap 函数：每次用 resampled residual 构造 y_star
  boot_max_demand <- function(data, i) {
    sampled_blocks <- sample(winter_blocks, length(winter_blocks), replace = TRUE)
    df_star <- do.call(rbind, sampled_blocks)
    df_star <- df_star[order(df_star$Date), ]
    
    resampled_resid <- df_star$resid
    if (length(resampled_resid) < n) {
      resampled_resid <- rep(resampled_resid, length.out = n)
    }
    resampled_resid <- resampled_resid[1:n]
    
    # y_star = predicted_fixed + residual
    y_star <- predicted_fixed + resampled_resid[1:length(predicted_fixed)]
    
    return(max(y_star, na.rm = TRUE))
  }
  
  # 执行 bootstrap
  set.seed(123)
  boot_out <- boot(data = demand_df, statistic = boot_max_demand, R = boot_iter)
  
  # 计算 95% CI
  ci <- boot.ci(boot_out, type = "perc")
  lower <- ci$perc[4]
  upper <- ci$perc[5]
  
  # 记录结果
  results <- rbind(results, data.frame(
    year = yr,
    max_predicted_demand = mean(boot_out$t),
    lower_95 = lower,
    upper_95 = upper
  ))
}

# 查看结果
print(results)

#可视化
# 真实 2013 冬天最大需求
real_max_demand <- max(demand_df$demand_gross, na.rm = TRUE, start_year ==2013)
# 使用 2013 天气数据预测的最大值（即不替换天气的版本）
original_predicted <- predict(model_TE, newdata = winter_df)
predicted_2013_demand <- max(original_predicted, na.rm = TRUE)

ggplot(results, aes(x = year)) +
  # 主线和误差线
  geom_line(aes(y = max_predicted_demand, color = "Max Predicted Demand"), size = 1) +
  geom_point(aes(y = max_predicted_demand, color = "Max Predicted Demand")) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95, color = "Max Predicted Demand"), width = 0.4) +
  
  # 添加水平线：真实和原始预测
  geom_hline(yintercept = real_max_demand, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = predicted_2013_demand, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("text", x = 1991.5, y = real_max_demand - 110, 
           label = "Actual 2013/14 Max Demand", color = "red", hjust = 0, size = 4) +
  annotate("text", x = 1991.5, y = predicted_2013_demand + 120, 
           label = "Model Prediction (2013/14 Weather)", color = "darkgreen", hjust = 0, size = 4) +
  annotate("text", x = 1997, y = 56800, 
           label = "Max predicted demand", color = "steelblue", hjust = 0, size = 5) +
  # 图例配置
  scale_color_manual(
    name = "Legend",
    values = c(
      "Max Predicted Demand" = "steelblue",
      "Actual 2013/14 Max Demand" = "red",
      "Model Prediction (2013/14 Weather)" = "darkgreen"
    )
  ) +
  
  labs(
    title = "Max Predicted Demand for 2013–14 Winter Using Historical Weather",
    x = "Weather Year Used (1991–2012)",
    y = "Max Predicted Demand (MW)"
  ) +
  theme_minimal()+
  theme(legend.position = "none")  




