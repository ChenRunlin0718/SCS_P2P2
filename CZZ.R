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
ggplot(demand_modelling, aes(x = `demand_gross`)) +
  geom_histogram(bins = 80, fill = "blue", alpha = 0.5) +
  labs(title = "Peak Daily Demand Distribution",
       x = "Demand gross (MW)", y = "Frequency")+
  theme_bw()
ggplot(demand_modelling, aes(x = 'temp', y = `demand_gross`)) +
  geom_point(aes(x = 'temp', y = `demand_gross`))



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
  select(demand_gross, TE, temp, TO, avg_temp, max_temp, min_temp, temp_range)
colSums(is.na(cor_data))
cor_matrix <- cor(cor_data, use = "complete.obs")

# 可视化相关矩阵
corrplot(cor_matrix, method = "color", addCoef.col = "black")

