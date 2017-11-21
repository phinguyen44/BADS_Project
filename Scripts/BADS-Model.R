################################################################################
# BADS-Model.R
#
################################################################################
# Description:
# BADS project - use cleaned data set, do minor cleaning, and build model
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(glmnet, caret, splines, broom, data.table, 
      mice, tidyverse, lubridate, ggplot2, grid, gridExtra, scales,
      ROCR)

source("Scripts/Helpful.R")

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

################################################################################
# CLEAN

df.clean <- df.train %>% 
  rename(user_birth_date = user_dob,
         item_brand_id   = brand_id) %>% 
  mutate(item_id         = factor(item_id),
         item_brand_id   = factor(item_brand_id),
         user_id         = factor(user_id),
         user_birth_date = ymd(user_birth_date),
         user_reg_date   = ymd(user_reg_date),
         order_date      = ymd(order_date),
         delivery_date   = ymd(delivery_date), 
         user_age        = floor(
           as.numeric((Sys.Date() - user_birth_date))/365),
         days_to_deliv   = as.numeric(delivery_date - order_date),
         days_from_open  = as.numeric(order_date - user_reg_date),
         order_day       = factor(weekdays(order_date)),
         order_month     = factor(months(order_date))) %>% 
  select(user_age, user_state, user_title,
         days_to_deliv, order_day, order_month,
         item_price, 
         return)

################################################################################
# IMPUTATION

# NA values
df.clean$days_to_deliv[df.clean$days_to_deliv < 0] <- NA
df.clean$user_age[df.clean$user_age >= 116] <- NA
df.clean$user_title[df.clean$user_title == "not reported"] <- NA

# Imputation
df.final <- df.clean[complete.cases(df.clean), ]

# Imputation via random sample
df.final2 <- df.clean

samplefxn <- function(df, var) {
  idx <- is.na(df[[var]])
  len <- sum(idx)
  
  values    <- sample(df[!idx, var], size = len, replace = TRUE)
  return(values)
}

df.final2$user_age[is.na(df.final2$user_age)]           <- samplefxn(
  df.final2, "user_age")
df.final2$days_to_deliv[is.na(df.final2$days_to_deliv)] <- samplefxn(
  df.final2, "days_to_deliv")
df.final2$user_title[is.na(df.final2$user_title)]       <- samplefxn(
  df.final2, "user_title")

# DIFFERENT IMPUTATION METHODS GIVE WILDLY DIFFERENT RESULTS

################################################################################
# MODEL CREATION (first pass)

# TODO: Try different data sets / models

mod <- return ~ . + order_day*order_month - order_day - order_month
# mod <- return ~ .
# mod <- return ~ . - order_day - order_month

# initial sample
samplex    <- sample(1:nrow(df.final), 10000, replace = FALSE)
testset    <- df.final[samplex, ]
trainset   <- df.final[-samplex, ]
trainset.x <- trainset %>% select(-return)

# Build a GLM model
final <- build.glm(mod, trainset, testset, alpha = 1)

# View results
p <- ggplot(data = final$Results, aes(prob, color = as.factor(actual))) + 
  geom_density(size = 1) +
  geom_vline(aes(xintercept = 0.5), color = "blue") + 
  labs(title = "Training Set Predicted Score") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank())
p

final$ClassTable
final$MCE

# TODO: Solve FNR issues
# false negative means we predict they don't return, but they do
# false positive means we predict they return, but they don't

# TODO: separate models for user_title to handle class imbalance issues

################################################################################
# CROSS-VALIDATION

# TODO: cross-validation

################################################################################
# Model evaluation

# ROC curve (determine optimal cutoff point)

# TODO: See if this actually works...

# get average penalties for fp and fn
final$Results$Class <- with(data = final$Results, ifelse(
  actual == result & actual == 1, "TP", ifelse(
    actual == result & actual == 0, "TN", ifelse(
      actual != result & actual == 1, "FN", "FP") 
    )
  ) 
)

final$Results <- final$Results %>% 
  bind_cols(testset) %>% 
  select(prob, actual, result, Class,
         item_price)

penalties <- final$Results %>% 
  group_by(Class) %>% 
  summarize(averages = mean(item_price))

fp_pen <- penalties$averages[penalties$Class == "FP"]
fn_pen <- penalties$averages[penalties$Class == "FN"]

cost_fn <- round(0.5*(3 + 0.1*fn_pen))
cost_fp <- round(0.5*fp_pen)

# find best cutoff
graphics.off()
roc_info <- ROCInfo(data = final$Results, predict = "prob", 
                    actual = "actual", cost.fp = 1, cost.fn = 1)
grid.draw(roc_info$plot)
