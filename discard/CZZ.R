library(readr)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(lubridate)

demand_modelling <- read_csv("SCS_demand_modelling.csv")
hourly_temp <- read_csv("SCS_hourly_temp.csv")
# 查看数据结构
#str(demand_modelling)
# 确保主数据集的 Date 是 Date 类型
demand_modelling$Date <- as.Date(demand_modelling$Date)
# 确保温度数据集的 Date 也是 Date 类型
# 转换 `Date` 列为 Date 类型，去掉时间部分
hourly_temp <- hourly_temp %>%
  mutate(
    Hour = hour(dmy_hm(Date)),  # 提取小时部分,
    Date = as.Date(Date, format = "%d/%m/%Y %H:%M") # 提取日期部分
  )
# ---- EDA ----
# 需求数据的分布
ggplot(demand_modelling, aes(x = `demand_gross`)) +
  geom_histogram(bins = 80, fill = "blue", alpha = 0.5) +
  labs(title = "Peak Daily Demand Distribution",
       x = "Demand gross (MW)", y = "Frequency")+
  theme_bw()

# 需求量 vs 温度变量
ggplot(demand_modelling, aes(x = TE, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Demand vs TE", x = "TE", y = "Demand gross (MW)")

ggplot(demand_modelling, aes(x = temp, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Demand vs temp", x = "temp", y = "Demand gross (MW)")



# ---- Q1 ----
# How does peak daily demand depend on temperature?
# 把小时的温度转换

# 计算每日温度统计量
temp_summary <- hourly_temp %>%
  group_by(Date) %>%
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),  # 24小时平均温度
    max_temp = max(temp, na.rm = TRUE),   # 最高温度
    min_temp = min(temp, na.rm = TRUE),   # 最低温度
    temp_range = max_temp - min_temp,     # 温度变化幅度
    last_3h_avg = mean(tail(temp, 3)),    # 过去3小时平均温度
    last_6h_avg = mean(tail(temp, 6)),    # 过去6小时平均温度
    last_12h_avg = mean(tail(temp, 12)),   # 过去12小时平均温度
    last_18h_avg = mean(tail(temp, 18)),   # 过去18小时平均温度
    avg_temp_4pm_11pm = mean(temp[Hour >= 18 & Hour <= 23], na.rm = TRUE)
  )

# 将新特征合并到主数据集
demand_modelling <- inner_join(demand_modelling, temp_summary, by = "Date")

cor_data <- demand_modelling  %>%
  select(demand_gross, TE, temp, TO, avg_temp, max_temp, min_temp, temp_range,
         last_3h_avg, last_6h_avg, last_12h_avg, last_18h_avg, avg_temp_4pm_11pm)
colSums(is.na(cor_data))
cor_matrix <- cor(cor_data, use = "complete.obs")

# 可视化相关矩阵
corrplot(cor_matrix, method = "color", addCoef.col = "black")

# 训练所有回归模型
model_TE <- lm(demand_gross ~ TE, data = demand_modelling)
model_temp <- lm(demand_gross ~ temp, data = demand_modelling)
model_TO <- lm(demand_gross ~ TO, data = demand_modelling)
model_avg_temp <- lm(demand_gross ~ avg_temp, data = demand_modelling)
model_max_temp <- lm(demand_gross ~ max_temp, data = demand_modelling)
model_min_temp <- lm(demand_gross ~ min_temp, data = demand_modelling)
model_temp_range <- lm(demand_gross ~ temp_range, data = demand_modelling)
model_combined <- lm(demand_gross ~ avg_temp + temp_range, data = demand_modelling)




# 计算所有模型的 R²
r2_values <- data.frame(
  Model = c("TE", "temp", "TO", "avg_temp", "max_temp", "min_temp", "temp_range", "avg_temp + temp_range"),
  R2 = c(
    summary(model_TE)$r.squared,
    summary(model_temp)$r.squared,
    summary(model_TO)$r.squared,
    summary(model_avg_temp)$r.squared,
    summary(model_max_temp)$r.squared,
    summary(model_min_temp)$r.squared,
    summary(model_temp_range)$r.squared,
    summary(model_combined)$r.squared
  )
)

# 按 R² 排序（降序）
r2_values <- r2_values %>%
  arrange(desc(R2))

# 打印 R² 值表格
print(r2_values)

# 计算 AIC（用于模型比较）
AIC(model_TE, model_temp, model_TO, model_avg_temp, model_max_temp,
    model_min_temp, model_temp_range, model_combined)

# 画残差图
par(mfrow=c(2,4))
plot(model_TE, which = 1, main = "Residuals: TE Model")
plot(model_temp, which = 1, main = "Residuals: temp Model")
plot(model_TO, which = 1, main = "Residuals: TO Model")
plot(model_avg_temp, which = 1, main = "Residuals: avg_temp Model")
plot(model_max_temp, which = 1, main = "Residuals: model_max_temp")
plot(model_min_temp, which = 1, main = "Residuals: model_min_temp")
plot(model_temp_range, which = 1, main = "Residuals: model_temp_range")
plot(model_combined, which = 1, main = "Residuals: Combined Model")
par(mfrow=c(1,1))

#'“我们的分析表明，NESO 目前使用的 TE 变量（R² = 0.0772，AIC = 69021.50）
#'是单变量中最好的选择。然而，使用组合变量 avg_temp + temp_range 
#'可以进一步提高预测能力（R² = 0.0811，AIC = 69008.78）。
#'因此，我们建议 NESO 在未来的预测模型中，
#'采用 avg_temp + temp_range 作为新的预测变量，以提高峰值电力需求的估计精度。”


# ---- Q2 ----
# How well does your model fit the historic data?
# 进一步更新模型

colnames(demand_modelling)
shapiro.test(demand_modelling$demand_gross)
shapiro.test(log(demand_modelling$demand_gross))

qqnorm(demand_modelling$demand_gross)
qqline(demand_modelling$demand_gross, col = "red")
# log版正态分布check
qqnorm(log(demand_modelling$demand_gross))
qqline(log(demand_modelling$demand_gross), col = "red")


lm_model <- lm(demand_gross ~ avg_temp + temp_range + wind * solar_S * monthindex,
               data = demand_modelling)
summary(lm_model)

lm_model_ploy <- lm(demand_gross ~ poly(avg_temp, 2) + poly(temp_range, 2) + 
                  wind + solar_S + poly(monthindex, 2) + wdayindex + poly(DSN, 2), data = demand_modelling)
summary(lm_model_ploy)

lm_model_interact <- lm(demand_gross ~ poly(avg_temp, 2) + poly(temp_range, 2) + 
                          solar_S * monthindex + poly(DSN, 2), 
                        data = demand_modelling)
summary(lm_model_interact)


lm_model_optimized <- lm(demand_gross ~ poly(avg_temp, 2) + poly(temp_range, 2) + 
                           solar_S + poly(monthindex, 2) + poly(DSN, 2), 
                         data = demand_modelling)
summary(lm_model_optimized)

glm_model <- glm(demand_gross ~ avg_temp + temp_range + wind + solar_S + monthindex + wdayindex + DSN, 
                 data = demand_modelling, family = Gamma(link = "log"))
summary(glm_model)

ggplot(demand_modelling, aes(x = Date, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Demand over year", x = "Year", y = "Demand gross (MW)")

ggplot(demand_modelling, aes(x = Date, y = demand_gross, color = factor(monthindex))) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", color = "red") +  
  scale_color_viridis_d(option = "cividis") +  # 颜色梯度
  labs(title = "Demand Change Over Year", x = "Year", y = "Demand Change", color = "Month") +
  theme_minimal()


ggplot(demand_modelling, aes(x = Date, y = TE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "TE change over year", x = "Year", y = "Temp Change")

library(viridis)  # 可选：用于更好的颜色梯度

ggplot(demand_modelling, aes(x = Date, y = TE, color = factor(monthindex))) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", color = "red") +  
  scale_color_viridis_d(option = "plasma") +  # 颜色梯度
  labs(title = "TE Change Over Year", x = "Year", y = "Temp Change", color = "Month") +
  theme_minimal()

ggplot(demand_modelling, aes(x = monthindex, y = demand_gross, color = factor(monthindex))) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", color = "red") +  
  #scale_color_viridis_d(option = "plasma") +  # 颜色梯度
  labs(title = "Demand Change Over TE", x = "TE", y = "Demand", color = "Month") +
  theme_minimal()

# 星期平均图
demand_modelling %>%
  group_by(wdayindex) %>%
  summarise(mean_demand = mean(demand_gross, na.rm = TRUE)) %>%
  mutate(wdayindex= factor(wdayindex, levels = c(0, 1, 2, 3, 4, 5, 6), 
                          labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  ggplot(aes(x = wdayindex, y = mean_demand, group = 1)) +
  geom_line(color = "red", size = 1.5, alpha = 0.5) +
  geom_point(color = "black", size = 2) + # 添加点以增强可视化
  labs(title = "Monthly Trend of Peak Demand", x = "Week (Winter)", y = "Average Peak Demand (MW)") +
  theme_light()

# 假期







