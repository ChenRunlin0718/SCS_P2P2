model_with_lag <- lm(demand_gross ~ lag1_demand + start_year + start_year:TE + wdayindex + 
                       solar_S + wind + DSN + I(DSN^2), data = demand_df)
colnames(demand_df)
library(dplyr)
library(lubridate)
library(tidyr)
# 添加 Month-Day 列用于匹配
demand_df$month_day <- format(as.Date(demand_df$Date), "%m-%d")

# 提取 2013-14 冬季数据
winter_df <- subset(demand_df, Date >= as.Date("2013-11-01") & Date <= as.Date("2014-03-31"))

# 创建 MM-DD 格式的日期用于匹配
winter_df$month_day <- format(winter_df$Date, "%m-%d")

# 找到 1991 年的同一天数据
weather_1991 <- subset(demand_df, year == "1992")
weather_1991$month_day <- format(weather_1991$Date, "%m-%d")

# 合并天气数据（使用左连接确保结构和顺序来自冬季数据）
merged_1991 <- merge(winter_df, weather_1991[, c("month_day", "TE", "solar_S", "wind")],
                   by = "month_day", all.x = TRUE, suffixes = c("", "_1991"))

# 替换字段
merged_1991$TE <- merged_1991$TE_1991
merged_1991$solar_S <- merged_1991$solar_S_1991
merged_1991$wind <- merged_1991$wind_1991

# 清理临时列
merged_1991 <- merged_1991[ , !(names(merged_1991) %in% c("TE_1991", "solar_S_1991", "wind_1991"))]
merged_1991$predicted_demand <- predict(model_with_lag, newdata = merged_1991)




# 初始化结果容器
results <- data.frame(year = integer(), max_predicted_demand = numeric())

# 遍历 1991 到 2012 年
for (yr in 1991:2012) {
  # 获取该年份天气数据
  weather_year <- subset(demand_df, year == yr)
  weather_year$month_day <- format(weather_year$Date, "%m-%d")
  
  # 合并：用 winter_df 结构 + 替换天气
  merged_df <- merge(winter_df, weather_year[, c("month_day", "TE", "solar_S", "wind")],
                     by = "month_day", all.x = TRUE, suffixes = c("", "_w"))
  
  # 替换天气变量
  merged_df$TE <- merged_df$TE_w
  merged_df$solar_S <- merged_df$solar_S_w
  merged_df$wind <- merged_df$wind_w
  
  # 删除临时列
  merged_df <- merged_df[, !(names(merged_df) %in% c("TE_w", "solar_S_w", "wind_w"))]
  
  # 预测
  merged_df$predicted_demand <- predict(model_with_lag, newdata = merged_df)
  
  # 记录最大 predicted_demand
  max_demand <- max(merged_df$predicted_demand, na.rm = TRUE)
  
  results <- rbind(results, data.frame(year = yr, max_predicted_demand = max_demand))
}

# 查看结果
print(results)


# 原数据
max(winter_df$demand_gross)
# 原预测
original_predicted<- predict(model_with_lag, newdata = winter_df)
max(original_predicted)


real_max_demand <- max(winter_df$demand_gross)
predicted_2013_demand <- max(original_predicted)


ggplot(results, aes(x = year, y = max_predicted_demand)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = real_max_demand, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = predicted_2013_demand, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("text", x = 1991.5, y = real_max_demand + 100, label = "Actual 2013/14 Max Demand", color = "red", hjust = 0) +
  annotate("text", x = 1991.5, y = predicted_2013_demand - 100, label = "Model Prediction (2013/14 Weather)", color = "darkgreen", hjust = 0) +
  labs(title = "Max Predicted Demand for 2013-14 Winter Using Historical Weather (1991–2012)",
       x = "Weather Year Used",
       y = "Max Predicted Demand (MW)")
  


# Boostrap 
# 设定 Bootstrap 迭代次数
n_bootstrap <- 1000
set.seed(123)
# 初始化结果表
results <- data.frame(year = integer(), 
                      max_predicted_demand = numeric(), 
                      lower_CI = numeric(), 
                      upper_CI = numeric())

for (yr in 1991:2012) {
  weather_year <- subset(demand_df, year == yr)
  weather_year$month_day <- format(weather_year$Date, "%m-%d")
  
  merged_df <- merge(winter_df, weather_year[, c("month_day", "TE", "solar_S", "wind")],
                     by = "month_day", all.x = TRUE, suffixes = c("", "_w"))
  
  merged_df$TE <- merged_df$TE_w
  merged_df$solar_S <- merged_df$solar_S_w
  merged_df$wind <- merged_df$wind_w
  merged_df <- merged_df[, !(names(merged_df) %in% c("TE_w", "solar_S_w", "wind_w"))]
  
  merged_df$predicted_demand <- predict(model_with_lag, newdata = merged_df)
  
  # Bootstrap
  boot_max <- replicate(n_bootstrap, {
    sample_preds <- sample(merged_df$predicted_demand, replace = TRUE)
    max(sample_preds, na.rm = TRUE)
  })
  
  # 中位数作为点估计，或也可以直接用原本的 max()
  max_demand <- mean(boot_max)
  lower_CI <- quantile(boot_max, 0.025, na.rm = TRUE)
  upper_CI <- quantile(boot_max, 0.975, na.rm = TRUE)
  
  results <- rbind(results, data.frame(year = yr, 
                                       max_predicted_demand = max_demand, 
                                       lower_CI = lower_CI, 
                                       upper_CI = upper_CI))
}

ggplot(results, aes(x = year, y = max_predicted_demand)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.4, color = "gray50") +
  geom_hline(yintercept = real_max_demand, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = predicted_2013_demand, linetype = "dashed", color = "darkgreen", size = 1) +
  annotate("text", x = 1991.5, y = real_max_demand + 100, label = "Actual 2013/14 Max Demand", color = "red", hjust = 0) +
  annotate("text", x = 1991.5, y = predicted_2013_demand - 100, label = "Model Prediction (2013/14 Weather)", color = "darkgreen", hjust = 0) +
  labs(title = "Max Predicted Demand for 2013-14 Winter Using Historical Weather (1991–2012)",
       x = "Weather Year Used",
       y = "Max Predicted Demand (MW)")


