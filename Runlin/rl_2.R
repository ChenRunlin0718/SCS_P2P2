# Assume your data frame is called demand_df 
# and has columns:
#   demand_gross    (dependent variable)
#   start_year      (integer winter label, e.g. 1990, 1991, etc.)
#   TE              (the TE_t variable)
#   DOW             (day-of-week indicator, e.g. 0=Sunday,...,6=Saturday)
#   DSN             (days since 1 November)
# Make sure 'start_year' and 'DOW' are set as factors.



demand_df <- read.csv("SCS_demand_modelling.csv", 
                      stringsAsFactors = FALSE)


############################################
# 1) Prepare the dataset
############################################
# Suppose your main dataset is called `demand_df`.
# It has columns: 
#   demand_gross, start_year, TE, wdayindex, DSN, etc.
# 
# Convert `start_year` to factor so each winter becomes a dummy,
# using 1990 as the baseline (reference) winter.
demand_df$start_year <- factor(demand_df$start_year)
demand_df$start_year <- relevel(demand_df$start_year, ref = "1991")

# Convert `wdayindex` to factor if you want day-of-week categories.
# We can set Sunday (0) as the reference.
demand_df$wdayindex <- factor(demand_df$wdayindex,
                              levels = c("0","1","2","3","4","5","6"))
demand_df$wdayindex <- relevel(demand_df$wdayindex, ref = "0")

############################################
# 2) Fit the model
############################################
# This formula structure encodes:
#   • An intercept + winter dummies (start_year)
#   • year-specific slopes for TE (start_year:TE)
#   • day-of-week categories (wdayindex)
#   • DSN and DSN^2 for a quadratic effect in "days since Nov 1"

model_1 <- lm(
  demand_gross ~ start_year + start_year:TE +
    wdayindex + DSN + I(DSN^2),
  data = demand_df
)

summary(model_1)

model_2 <- lm(
  demand_gross ~ start_year + start_year:TE + solar_S + wind +
    wdayindex + DSN + I(DSN^2),
  data = demand_df
)

summary(model_2)
