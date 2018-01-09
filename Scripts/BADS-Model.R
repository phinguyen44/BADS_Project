################################################################################
# BADS-Model.R
# 
# Phi Nguyen: phi.nguyen@outlook.com
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

source("Scripts/BADS.R")
source("Scripts/Helpful.R")

################################################################################
# MODEL CREATION (first pass)

mod <- return ~ . - order_day - order_month + order_day*order_month

# initial sample
samplex    <- sample(1:nrow(df.final), 20000, replace = FALSE)
testset    <- df.final[samplex, ]
trainset   <- df.final[-samplex, ]
trainset.x <- trainset %>% select(-return)

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

################################################################################
# CROSS-VALIDATION

# TODO: cross-validation

################################################################################
# MODEL EVALUATION

# TODO: Model evaluation

################################################################################
# GENERATE PREDICTION (on class set)

# TODO: if brand_id or item_id used, remember to convert those values

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
         order_month     = factor(months(order_date)))

df.test.c$days_to_deliv[df.test.c$days_to_deliv < 0]         <- NA
df.test.c$user_age[df.test.c$user_age >= 116]                <- NA
df.test.c$user_title[df.test.c$user_title == "not reported"] <- NA

df.test.c$user_age[is.na(df.test.c$user_age)]           <- samplefxn(
  df.test.c, "user_age", "mean")
df.test.c$days_to_deliv[is.na(df.test.c$days_to_deliv)] <- samplefxn(
  df.test.c, "days_to_deliv", "mean")
df.test.c$user_title[is.na(df.test.c$user_title)]       <- samplefxn(
  df.test.c, "user_title", "sample")

df.test.c$item_price_d <- assign.bins(df.test.c, price_disc, "item_price")
df.test.c$user_age_d   <- assign.bins(df.test.c, age_disc, "user_age")

df.test.c <- df.test.c %>% 
  select(order_item_id,
         user_age_d, user_state, user_title,
         days_to_deliv, days_from_open, order_day, order_month,
         item_price_d)

df.test.c$return <- 0

df.test.x <- df.test.c %>% select(-order_item_id)

# FINAL PREDICTIONS
f.matrix <- model.matrix(mod, data = df.test.x)
f.pred   <- predict(final$mod, newx = f.matrix, s = "lambda.1se", type = "response")

df.test.c$return <- f.pred
df.all           <- df.test.c %>% select(order_item_id, return)

# write to csv
write.csv2(df.all, file = "Results/526624_nguyen.csv", row.names = FALSE)
