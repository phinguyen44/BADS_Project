################################################################################
# BADS.R
#
# Phi Nguyen: phi.nguyen@outlook.com
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
# install.packages("needs")
# library(needs) # needs is utility that installs and loads missing packages
needs(glmnet, caret, splines, 
      tidyverse, lubridate, ggplot2)

source("Scripts/Helpful.R")

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

################################################################################
# DATA EXPLORATION

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

price_disc  <- discrete.bin(item_price, variable = "Var1", 10)

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

# TODO: Organize brands by return rates (WOE)

################################################################################
# DATA CLEANING AND IMPUTATION

# TODO: discretize price because otherwise it's too strong of a var in model

# TODO: Just a thought. Can days to deliv be included if a user would be shown
# a message to prevent a purchase in the cart stage? Meaning before a purchase
# is made?

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
daydf   <- return.check(df.clean, "order_day")
monthdf <- return.check(df.clean, "order_month")

delivdays <- num.check(df.clean, "days_to_deliv")
opendays  <- num.check(df.clean, "days_from_open")
user_age  <- num.check(df.clean, "user_age")

summary(df.clean)

# NA values
df.clean$days_to_deliv[df.clean$days_to_deliv < 0]         <- NA
df.clean$user_age[df.clean$user_age >= 116]                <- NA
df.clean$user_title[df.clean$user_title == "not reported"] <- NA

# Imputation via remove NA
# df.final <- df.clean[complete.cases(df.clean), ]

# Imputation via random sample for categorical and mean for numeric
df.final <- df.clean

df.final$user_age[is.na(df.final$user_age)]           <- samplefxn(
  df.final, "user_age", "mean")
df.final$days_to_deliv[is.na(df.final$days_to_deliv)] <- samplefxn(
  df.final, "days_to_deliv", "mean")
df.final$user_title[is.na(df.final$user_title)]       <- samplefxn(
  df.final, "user_title", "sample")

# TODO: DIFFERENT IMPUTATION METHODS GIVE WILDLY DIFFERENT RESULTS

# additional EDA
daydf     <- return.check(df.final, "order_day")
monthdf   <- return.check(df.final, "order_month")

delivdays <- num.check(df.final, "days_to_deliv")
user_age  <- num.check(df.final, "user_age")

age_disc  <- discrete.bin(user_age, variable = "Var1", 5)

p5 <- ggplot(data = age_disc, aes(x = bins, y = ReturnRate)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))
p5

# Add discrete item_price and user_age
df.final$item_price_d <- assign.bins(df.final, price_disc, "item_price")
df.final$user_age_d   <- assign.bins(df.final, age_disc, "user_age")

df.final <- df.final %>% 
  select(user_age_d, user_state, user_title,
         days_to_deliv, days_from_open, order_day, order_month,
         item_price_d,
         return)

keep <- c("df.train", "df.test", "df.final", "price_disc", "age_disc")
rm(list = ls()[!(ls() %in% keep)])