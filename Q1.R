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

kable(model_performance, col.names = c("Model", "Adjusted R²", "AIC", "MSE"),
      caption = "Comparison of Single-Variable Regression Models for Peak Demand")








