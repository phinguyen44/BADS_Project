#####################################################################################
# BADS-Model.R
#
#####################################################################################
# Description:
# BADS project - model only
# 
#####################################################################################

#####################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(glmnet, caret, splines,
      tidyverse, magrittr, lubridate, zoo, 
      ggplot2)

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

#####################################################################################
# CLEAN

# TODO: Update based on BADS.R

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
         user_age        = floor(as.numeric((Sys.Date() - user_birth_date))/365),
         days_to_deliv   = as.numeric(delivery_date - order_date),
         days_from_open  = as.numeric(order_date - user_reg_date),
         order_day       = factor(weekdays(order_date)),
         order_month     = factor(months(order_date))) %>% 
  select(user_id, 
         user_age, user_state, user_title,
         days_to_deliv, days_from_open, order_day, order_month,
         item_id, item_brand_id, item_price, return)

# NA values
df.clean$days_to_deliv[df.clean$days_to_deliv < 0] <- NA
df.clean$user_age[df.clean$user_age >= 116] <- NA
df.clean$user_title[df.clean$user_title == "not reported"] <- NA

# Imputation
df.final <- df.clean[complete.cases(df.clean), ]

#####################################################################################
# Model creation

simple.mod <- return ~ .

ageknots <- c(40, 65)
fun.mod <- return ~ 
  ns(user_age, knots = ageknots) + 
  user_state +
  user_title + 
  days_to_deliv + 
  days_from_open +
  order_day + 
  order_month +
  item_id + 
  item_brand_id + 
  I(log(item_price)) - 
  user_id

vars.in <- df.final %>% 
  select(-user_id)

samplex  <- sample(1:nrow(vars.in), 10000, replace = FALSE)
testset  <- vars.in[samplex, ]
trainset <- vars.in[-samplex, ]

testset2 <- testset %>% 
  select(-return)

matrixx   <- model.matrix(fun.mod, data = df.final)
mod1      <- cv.glmnet(x = matrixx, y = df.final$return, family = "binomial", standardize = TRUE)
plot(mod1.t)
mod1.t$lambda.min
coef(mod1.t, s = "lambda.min")

newx    <-  model.matrix(test.mod, data = testset)

start <- Sys.time()
mod2    <- glm(simple.mod, data = trainset, family = binomial(link = "logit"))
pred2 <- predict(mod2, type = "response")
check2 <- table(predicted = round(pred2), actual = testset$return)
mce2 <- 1 - sum(diag(check2)) / sum(check2)
end <- Sys.time()
end - start

# TODO: fix error in model.frame.default: factor has new levels
# https://stackoverflow.com/questions/29873178/error-in-model-frame-default-for-predict-factor-has-new-levels-for-a-cha
# In summary, the error basically means that the model is unable to make predictions for unknown levels in the testdata that were never encountered during the training of the model.

pred1 <- predict(mod1.t, newx = newx, s = "lambda.min", type = "response")

test <- round(cbind(pred1, pred2))

# check which is better
check1 <- table(predicted = test[, 1], actual = testset$return)

mce1 <- 1 - sum(diag(check1)) / sum(check1)

# TODO: Consider log transform of item_price
# TODO: Consider spline for age
# TODO: standardize numeric variables

# GARBAGE MODEL :(

# try a few candidate models
# consider separate models for user_title to handle class imbalance issues

#####################################################################################
# Model evaluation

# ROC curve (after doing training / test set)
# 
# do cross-validation?
