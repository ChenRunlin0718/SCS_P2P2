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
# 需求量 vs 温度变量
ggplot(demand_modelling, aes(x = TE, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Demand vs TE", x = "TE", y = "Demand gross (MW)")


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
    last_12h_avg = mean(tail(temp, 12))   # 过去12小时平均温度
  )

# 将新特征合并到主数据集
demand_modelling <- inner_join(demand_modelling, temp_summary, by = "Date")

cor_data <- demand_modelling  %>%
  select(demand_gross, TE, temp, TO, avg_temp, max_temp, min_temp, temp_range,
         last_3h_avg, last_6h_avg, last_12h_avg)
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





#'“我们的分析表明，NESO 目前使用的 TE 变量（R² = 0.0772，AIC = 69021.50）
#'是单变量中最好的选择。然而，使用组合变量 avg_temp + temp_range 
#'可以进一步提高预测能力（R² = 0.0811，AIC = 69008.78）。
#'因此，我们建议 NESO 在未来的预测模型中，
#'采用 avg_temp + temp_range 作为新的预测变量，以提高峰值电力需求的估计精度。”
#'


# ---- Q2 ----


 


# ---- Q3 ----


# ---- Q4 ----


