################################################################################
# BADS.R
#
################################################################################
# Description:
# BADS project. Mostly data exploration.
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(glmnet, caret, splines,
      tidyverse, magrittr, lubridate, zoo, 
      ggplot2)

source("Scripts/Helpful.R")

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

################################################################################
# Data exploration

fxns.fac  <- list(dist.check = dist.check, return.check = return.check)
fxns.num  <- list(dist.check = dist.check, return.check = num.check)

# factor variables
item_color <- lapply(fxns.fac, function(f) f(df.train, "item_color"))
item_size  <- lapply(fxns.fac, function(f) f(df.train, "item_size"))
user_title <- lapply(fxns.fac, function(f) f(df.train, "user_title"))
user_state <- lapply(fxns.fac, function(f) f(df.train, "user_state"))

# numeric variables
item_id    <- lapply(fxns.num, function(f) f(df.train, "item_id"))
brand_id   <- lapply(fxns.num, function(f) f(df.train, "brand_id"))
user_id    <- lapply(fxns.num, function(f) f(df.train, "user_id"))
item_price <- num.check(df.train, "item_price")

price_disc  <- discrete.bin(item_price, variable = "Var1", 40)

# check distribution (numeric): item_price
p <- ggplot() +
  geom_density(data = df.train, aes(x = item_price), 
               color = "red", alpha = 0.2, adjust = 4) +
  geom_density(data = df.test, aes(x = item_price), 
               color = "blue", alpha = 0.2, adjust = 4) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank())
p

# check item_price return rates
p2 <- ggplot(data = price_disc, aes(x = bins, y = ReturnRate)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))
p2

# brand_disc  <- discrete.bin(brand_id$return.check, variable = "Total", 20)
# brand_disc2 <- discrete.power(brand_id$return.check, 
#                               variable = "Total", powerval = 2)
# # check brand_disc return rates
# p3 <- ggplot(data = brand_disc, aes(x = bins, y = ReturnRate)) + 
#   geom_bar(stat = "identity") + 
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank()) + 
#   theme(panel.grid.major.x = element_blank()) +
#   theme(axis.text.x = element_text(angle = 90))
# p3

# TODO: Organize brands by return rates

################################################################################
# Data cleaning & imputation

# TODO: add new variables (discrete item_price, discrete age, brand_id)

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
         order_month     = factor(months(order_date))) 

# additional EDA
# TODO: do month/doy variables pass the sanity check
daydf   <- return.check(df.clean, "order_day")
monthdf <- return.check(df.clean, "order_month")

delivdays <- num.check(df.clean, "days_to_deliv")
# opendays  <- num.check(df.clean, "days_from_open")
user_age  <- num.check(df.clean, "user_age")

summary(df.clean)

# wth are people buying in march?

df.clean <- df.clean %>% 
  select(user_age, user_state, user_title,
         days_to_deliv, order_day, order_month,
         item_price, 
         return)