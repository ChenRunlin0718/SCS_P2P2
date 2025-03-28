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
# ----EDA----
# Demand Distribution Plot 
p_demand_hist <- ggplot(demand_modelling, aes(x = demand_gross)) +
  geom_histogram(binwidth = 300, fill = "blue", alpha = 0.5) +
  labs(title = "Peak Daily Demand Distribution", x = "Demand (MW)", y = "Frequency") +
  theme_bw()

# Normality check
p_qqplot <- ggplot() +
  geom_qq(aes(sample = demand_modelling$demand_gross)) +
  geom_qq_line(aes(sample = demand_modelling$demand_gross), col = "red") +
  labs(title = "Q-Q Plot for Peak Demand")+
  theme_bw()

# demand combine 
p_demand_hist + p_qqplot


# Temperature Distribution
ggplot(demand_modelling, aes(x = temp)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.5) +
  labs(title = "Temperature Distribution", x = "Temperature (°C)", y = "Frequency") +
  theme_bw()


# Demand Trend Over Time
demand_modelling %>%
  mutate(monthindex = factor(monthindex, levels = c(10, 11, 0, 1, 2), 
                             labels = c("Nov", "Dec", "Jan", "Feb", "Mar"))) %>%
  ggplot(aes(x = Date, y = demand_gross, color = factor(monthindex))) +
  geom_point(alpha = 0.5, size = 1) +  
  geom_smooth(method = "lm", color = "red") +  
  labs(title = "Demand Change Over Year", x = "Year", y = "Demand Change", color = "Month") +
  theme_minimal()

# Average Monthly Trend (demand and Temperature)
p_demand <- demand_modelling %>%
  group_by(monthindex) %>%
  summarise(mean_demand = mean(demand_gross, na.rm = TRUE)) %>%
  mutate(monthindex = factor(monthindex, levels = c(10, 11, 0, 1, 2), 
                             labels = c("Nov", "Dec", "Jan", "Feb", "Mar"))) %>%
  ggplot(aes(x = monthindex, y = mean_demand, group = 1)) +
  geom_line(color = "red", size = 1.5, alpha = 0.5) +
  geom_point(color = "black", size = 2) + # 添加点以增强可视化
  labs(title = "Monthly Trend of Peak Demand", x = "Month (Winter)", y = "Average Peak Demand (MW)") +
  theme_light()

p_temp <- demand_modelling %>%
  group_by(monthindex) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>%
  mutate(monthindex = factor(monthindex, levels = c(10, 11, 0, 1, 2), 
                             labels = c("Nov", "Dec", "Jan", "Feb", "Mar"))) %>%
  ggplot(aes(x = monthindex, y = mean_temp, group = 1)) +
  geom_line(color = "blue", size = 1.5, alpha = 0.5) +
  geom_point(color = "black", size = 2) + 
  labs(title = "Monthly Temperature Change", x = "Month (Winter)", y = "Average Temperature(°C)") +
  theme_light()
p_demand + p_temp



# Relationship Between Other Factors and Demand
# Wind
p_wind <- ggplot(demand_modelling, aes(x = wind, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Wind Generation vs Demand", x = "Wind Capacity Factor", y = "Peak Demand (MW)")

# Solar
p_solar <- ggplot(demand_modelling, aes(x = solar_S, y = demand_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Solar Generation vs Demand", x = "Solar Capacity Factor", y = "Peak Demand (MW)")

p_wind + p_solar #combine two plots




# ---- Q1-----
# sec 2.1
# Compute correlation between temperature variables and demand
cor_data <- demand_modelling %>%
  select(demand_gross, temp, TE, TO)

cor_matrix <- cor(cor_data, use = "complete.obs")

# Convert correlation matrix into a table format
cor_table <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 == "demand_gross") %>%
  select(Var2, Freq) %>%
  rename(Temperature_Metric = Var2, Correlation_with_Demand = Freq)

# Print the table in a well-formatted way
kable(cor_table, format = "latex", col.names = c("Temperature Measurements", "Correlation with Demand"), 
      caption = "Correlation Between Temperature and Demand")

# sec 2.2
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

# Daily
cor_data <- demand_modelling %>%
  select(demand_gross, avg_temp, max_temp, min_temp, temp_range)

cor_matrix <- cor(cor_data, use = "complete.obs")

# Convert correlation matrix into a table format
cor_table <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 == "demand_gross") %>%
  select(Var2, Freq) %>%
  rename(Temperature_Metric = Var2, Correlation_with_Demand = Freq)

# Print the table in a well-formatted way
kable(cor_table, format = "latex", col.names = c("Temperature Measurements", "Correlation with Demand"), 
      caption = "Correlation Between Temperature and Demand")



# Hourly
cor_data <- demand_modelling %>%
  select(demand_gross, last_3h_avg, last_6h_avg, last_12h_avg, last_18h_avg)

cor_matrix <- cor(cor_data, use = "complete.obs")

# Convert correlation matrix into a table format
cor_table <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 == "demand_gross") %>%
  select(Var2, Freq) %>%
  rename(Temperature_Metric = Var2, Correlation_with_Demand = Freq)

# Print the table in a well-formatted way
kable(cor_table, format = "latex", col.names = c("Temperature Measurements", "Correlation with Demand"), 
      caption = "Correlation Between Temperature and Demand")

# Day & Night Time
cor_data <- demand_modelling %>%
  select(demand_gross, day_temp, night_temp)

cor_matrix <- cor(cor_data, use = "complete.obs")

# Convert correlation matrix into a table format
cor_table <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 == "demand_gross") %>%
  select(Var2, Freq) %>%
  rename(Temperature_Metric = Var2, Correlation_with_Demand = Freq)

# Print the table in a well-formatted way
kable(cor_table,format = "latex", col.names = c("Temperature Measurements", "Correlation with Demand"), 
      caption = "Correlation Between Temperature and Demand")



# sec 2.3
# Fit single-variable regression models
model_TE <- lm(demand_gross ~ TE, data = demand_modelling)
model_temp <- lm(demand_gross ~ temp, data = demand_modelling)
model_TO <- lm(demand_gross ~ TO, data = demand_modelling)

model_avg_temp <- lm(demand_gross ~ avg_temp, data = demand_modelling)
model_max_temp <- lm(demand_gross ~ max_temp, data = demand_modelling)
model_min_temp <- lm(demand_gross ~ min_temp, data = demand_modelling)
model_temp_range <- lm(demand_gross ~ temp_range, data = demand_modelling)

model_3h_avg <- lm(demand_gross ~ last_3h_avg, data = demand_modelling)
model_6h_avg <- lm(demand_gross ~ last_6h_avg, data = demand_modelling)
model_12h_avg <- lm(demand_gross ~ last_12h_avg, data = demand_modelling)
model_18h_avg <- lm(demand_gross ~ last_18h_avg, data = demand_modelling)

model_day_temp <- lm(demand_gross ~ day_temp, data = demand_modelling)
model_night_temp <- lm(demand_gross ~ night_temp, data = demand_modelling)


model_performance <- data.frame(
  Model = c("TE", "Temp", "TO", "avg_temp", "max_temp", "min_temp", "temp_range",
            "last_3h_avg", "last_6h_avg", "last_12h_avg", "last_18h_avg",
            "day_temp", "night_temp"),
  
  R2 = c(summary(model_TE)$r.squared, summary(model_temp)$r.squared, summary(model_TO)$r.squared,
         summary(model_avg_temp)$r.squared, summary(model_max_temp)$r.squared, summary(model_min_temp)$r.squared,
         summary(model_temp_range)$r.squared, summary(model_3h_avg)$r.squared, summary(model_6h_avg)$r.squared,
         summary(model_12h_avg)$r.squared, summary(model_18h_avg)$r.squared, 
         summary(model_day_temp)$r.squared, summary(model_night_temp)$r.squared),
  
  AIC = c(AIC(model_TE), AIC(model_temp), AIC(model_TO),
          AIC(model_avg_temp), AIC(model_max_temp), AIC(model_min_temp),
          AIC(model_temp_range), AIC(model_3h_avg), AIC(model_6h_avg),
          AIC(model_12h_avg), AIC(model_18h_avg), 
          AIC(model_day_temp), AIC(model_night_temp))
)
# Sort by R² to find the best models
model_performance <- model_performance[order(-model_performance$R2), ]

kable(model_performance, format = "latex", col.names = c("Model", "R²", "AIC"),
      caption = "Comparison of Single-Variable Regression Models for Peak Demand")


# ----Q2----

# ----Q3----

# ----Q4----



