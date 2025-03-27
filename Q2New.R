library(boot)
# ---- Current Best Model----
model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                 wdayindex + solar_S + wind + DSN + I(DSN^2),
               data = demand_df)
summary(model_TE)
# Residuals Plot
par(mfrow=c(2,2))
plot(model_TE)

# -----Check Autocorrelation----
# Residual over time
par(mfrow=c(1,1))
plot(c(1:3221), model_TE$residuals, type="l", xlab="Time", ylab="Residuals")

acf(model_TE$residuals, main="Autocorrelation")

#------Bootstrapping----
# 设置 block size
block_size <- 14
n <- nrow(demand_df)
num_blocks <- ceiling(n / block_size)

# 拟合原始模型
model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                 wdayindex + solar_S + wind + DSN + I(DSN^2),
               data = demand_df)

# 提取残差与拟合值
resid_original <- residuals(model_TE)
fitted_values <- fitted(model_TE)

# 把残差分块（不重叠 block）
resid_blocks <- split(resid_original, ceiling(seq_along(resid_original) / block_size))

library(boot)

boot_block_resid <- function(data, i) {
  # 重采样区块
  sampled_blocks <- sample(resid_blocks, num_blocks, replace = TRUE)
  resid_star_full <- unlist(sampled_blocks)
  
  # 若长度不足，重复补足；若过长，截断
  if (length(resid_star_full) < n) {
    resid_star_full <- rep(resid_star_full, length.out = n)
  }
  resid_star <- resid_star_full[1:n]
  
  # 构造新的响应变量
  y_star <- fitted_values + resid_star
  data$y_star <- y_star
  
  # 用新的 y_star 拟合模型
  model_star <- lm(y_star ~ start_year + start_year:TE + 
                     wdayindex + solar_S + wind + DSN + I(DSN^2),
                   data = data)
  
  return(coef(model_star))
}

set.seed(123)
boot_results <- boot(data = demand_df, statistic = boot_block_resid, R = 1000)



# 查看某个系数的分布，例如第2个（start_year）
hist(boot_results$t[, 2], main = "Bootstrap Distribution of start_year", xlab = "Coefficient")

# 查看95%置信区间
boot.ci(boot_results, type = "perc", index = 2)  # index 从1开始

summary(model_TE)$coefficients  # OLS估计与标准误

apply(boot_results$t, 2, sd)    # Bootstrap 标准误


# 比如对比 boot_results_7 和 boot_results_14
block_size <- 7
boot_results_7 <- boot(data = demand_df, statistic = boot_block_resid, R = 1000)   # 用 block_size = 7 的结果
block_size <- 14
boot_results_14 <- boot(data = demand_df, statistic = boot_block_resid, R = 1000)  # 用 block_size = 14 的结果

# 计算每个系数的均值和标准差
mean_7 <- colMeans(boot_results_7$t)
mean_14 <- colMeans(boot_results_14$t)

sd_7 <- apply(boot_results_7$t, 2, sd)
sd_14 <- apply(boot_results_14$t, 2, sd)

# 打印对比表格
result_compare <- data.frame(
  Coef = names(coef(model_TE)),
  Mean_block7 = mean_7,
  Mean_block14 = mean_14,
  SD_block7 = sd_7,
  SD_block14 = sd_14
)
print(result_compare)



orig_coef <- coef(model_TE)
boot_means <- colMeans(boot_results$t)
boot_sd <- apply(boot_results$t, 2, sd)

compare_table <- data.frame(
  Coefficient = names(orig_coef),
  OLS = orig_coef,
  Boot_mean = boot_means,
  Boot_SD = boot_sd
)
print(compare_table)
hist(boot_results$t[, 2], breaks = 40, main = "Bootstrap for start_year", xlab = "Coefficient")
abline(v = coef(model_TE)[2], col = "red", lwd = 2)  # OLS估计

# ----三种对比-----
block_sizes <- c(7, 14, 30)
boot_list <- list()

for (b in block_sizes) {
  block_size <- b
  n <- nrow(demand_df)
  num_blocks <- ceiling(n / block_size)
  
  model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                   wdayindex + solar_S + wind + DSN + I(DSN^2),
                 data = demand_df)
  resid_original <- residuals(model_TE)
  fitted_values <- fitted(model_TE)
  resid_blocks <- split(resid_original, ceiling(seq_along(resid_original) / block_size))
  
  boot_block_resid <- function(data, i) {
    sampled_blocks <- sample(resid_blocks, num_blocks, replace = TRUE)
    resid_star_full <- unlist(sampled_blocks)
    if (length(resid_star_full) < n) {
      resid_star_full <- rep(resid_star_full, length.out = n)
    }
    resid_star <- resid_star_full[1:n]
    y_star <- fitted_values + resid_star
    data$y_star <- y_star
    model_star <- lm(y_star ~ start_year + start_year:TE + 
                       wdayindex + solar_S + wind + DSN + I(DSN^2),
                     data = data)
    return(coef(model_star))
  }
  
  set.seed(123)
  boot_result <- boot(data = demand_df, statistic = boot_block_resid, R = 500)
  boot_list[[as.character(b)]] <- boot_result$t
}



library(reshape2)
library(dplyr)

# 合并成一个大的 data frame
combined_df <- data.frame()

for (b in names(boot_list)) {
  boot_matrix <- boot_list[[b]]
  df_b <- as.data.frame(boot_matrix)
  df_b$BlockSize <- b
  combined_df <- rbind(combined_df, df_b)
}

# 重命名列为系数名
colnames(combined_df)[1:length(coef(model_TE))] <- names(coef(model_TE))

# 转成长格式（long format）
long_df <- melt(combined_df, id.vars = "BlockSize", variable.name = "Coefficient", value.name = "Estimate")
library(ggplot2)

ggplot(long_df, aes(x = BlockSize, y = Estimate, fill = BlockSize)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ Coefficient, scales = "free_y") +
  theme_bw() +
  theme(strip.text = element_text(size = 10)) +
  labs(title = "Bootstrap Coefficients Across Block Sizes",
       y = "Coefficient Estimate", x = "Block Size")





# ----all-----

model_TE <- lm(demand_gross ~ start_year + start_year:TE + 
                 wdayindex + solar_S + wind + DSN + I(DSN^2),
               data = demand_df)

demand_df$resid <- residuals(model_TE)
demand_df$fitted <- fitted(model_TE)
winter_blocks <- split(demand_df, demand_df$start_year)
n <- nrow(demand_df)


boot_block_resid <- function(data, i) {
  sampled_blocks <- sample(winter_blocks, length(winter_blocks), replace = TRUE)
  df_star <- do.call(rbind, sampled_blocks)
  df_star <- df_star[order(df_star$Date), ]  # 可选，保持时间顺序
  
  resid_star_full <- df_star$resid
  
  # 若残差不够，重复补足；若太多，截断
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
colMeans(boot_results2$t)  # 对比两组均值是否接近

# 查看标准误差
apply(boot_results$t, 2, sd)

# 与原模型系数对比
coef(model_TE)

# 查看95%置信区间
boot.ci(boot_results, type = "perc", index = 2)

# 可视化某个系数
hist(boot_results$t[, 2], main = "start_year", xlab = "Coefficient")
abline(v = coef(model_TE)[2], col = "red", lwd = 2)


# 原始 OLS 系数
ols_coef <- coef(model_TE)

# bootstrap 估计
boot_mean <- colMeans(boot_results$t)
boot_sd <- apply(boot_results$t, 2, sd)

# 系数名字
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

# 转成数据框
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







# -----Q3-----

# 初始化结果容器
library(boot)

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
real_max_demand <- max(subset(demand_df, start_year == 2013)$demand_gross, na.rm = TRUE)
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







