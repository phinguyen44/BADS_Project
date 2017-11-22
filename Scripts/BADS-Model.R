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

# TODO: Just a thought. Can days to deliv be included if a user would be shown
# a message to prevent a purchase in the cart stage? Meaning before a purchase
# is made?

################################################################################
# IMPUTATION

# NA values
df.clean$days_to_deliv[df.clean$days_to_deliv < 0] <- NA
df.clean$user_age[df.clean$user_age >= 116] <- NA
df.clean$user_title[df.clean$user_title == "not reported"] <- NA

# Imputation via remove NA
# df.final <- df.clean[complete.cases(df.clean), ]

# Imputation via random sample for categorical and mean for numeric
df.final <- df.clean

samplefxn <- function(df, var, type) {
  idx <- is.na(df[[var]])
  len <- sum(idx)

  values <- switch(type,
                   sample = sample(df[!idx, var], size = len, replace = TRUE),
                   mean   = rep(round(mean(df[[var]], na.rm = TRUE)), 
                                times = len))

  return(values)
}

df.final$user_age[is.na(df.final$user_age)]           <- samplefxn(
  df.final, "user_age", "mean")
df.final$days_to_deliv[is.na(df.final$days_to_deliv)] <- samplefxn(
  df.final, "days_to_deliv", "mean")
df.final$user_title[is.na(df.final$user_title)]       <- samplefxn(
  df.final, "user_title", "sample")

# DIFFERENT IMPUTATION METHODS GIVE WILDLY DIFFERENT RESULTS

################################################################################
# MODEL CREATION (first pass)

mod <- return ~ . - order_day - order_month - days_to_deliv +
  order_day*order_month
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

FPR <- final$FPR
FNR <- final$FNR

FPR
FNR

testset$returnProb <- final$Results[["prob"]]

# TODO: Solve FPR / FNR issues ()
# false negative means we predict they don't return, but they do
# false positive means we predict they return, but they don't (higher penalty!
# because it results in a lost sale)

# TODO: separate models for user_title to handle class imbalance issues

################################################################################
# CROSS-VALIDATION

# TODO: cross-validation

################################################################################
# Model evaluation

# ROC curve with penalty (determine optimal cutoff point)

# TODO: Am I doing this right? or am I mixing up FPR / FNR penalties

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
                    actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn)
grid.draw(roc_info$plot)

################################################################################
# GENERATE PREDICTION (on class set)

# Clean class set
df.test.c <- df.test %>% 
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
  select(order_item_id, user_age, user_state, user_title,
         days_to_deliv, order_day, order_month,
         item_price)

df.test.c$days_to_deliv[df.test.c$days_to_deliv < 0] <- NA
df.test.c$user_age[df.test.c$user_age >= 116] <- NA
df.test.c$user_title[df.test.c$user_title == "not reported"] <- NA

df.test.c$user_age[is.na(df.test.c$user_age)]           <- samplefxn(
  df.test.c, "user_age", "mean")
df.test.c$days_to_deliv[is.na(df.test.c$days_to_deliv)] <- samplefxn(
  df.test.c, "days_to_deliv", "mean")
df.test.c$user_title[is.na(df.test.c$user_title)]       <- samplefxn(
  df.test.c, "user_title", "sample")

df.test.c$return <- 0

df.test.x <- df.test.c %>% select(-order_item_id)

# FINAL PREDICTIONS
f.matrix <- model.matrix(mod, data = df.test.x)
f.pred   <- predict(final$mod, newx = f.matrix, s = "lambda.1se", type = "response")

df.test.c$return <- f.pred
df.all           <- df.test.c %>% select(order_item_id, return)

# write to csv
write.csv(df.all, file = "526624_nguyen.csv", row.names = FALSE)