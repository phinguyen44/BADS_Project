#####################################################################################
# BADS.R
#
#####################################################################################
# Description:
# BADS project
# 
#####################################################################################

#####################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(glmnet, caret, 
      tidyverse, magrittr, lubridate, zoo, 
      ggplot2)

source("Scripts/Helpful.R")

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

#####################################################################################
# Data exploration

fxns.fac  <- list(dist.check = dist.check, return.check = return.check)
fxns.num  <- list(dist.check = dist.check, return.check = num.check)

# factor variables
item_color <- lapply(fxns.fac, function(f) f("item_color"))
item_size  <- lapply(fxns.fac, function(f) f("item_size"))
user_title <- lapply(fxns.fac, function(f) f("user_title"))
user_state <- lapply(fxns.fac, function(f) f("user_state"))

# numeric variables
item_id    <- lapply(fxns.num, function(f) f("item_id"))
user_id    <- lapply(fxns.num, function(f) f("user_id"))
item_price <- num.check("item_price")

price_disc <- discret(item_price, 10)

# check distribution (numeric): item_price
p <- ggplot() +
  geom_density(data = df.train, aes(x = item_price), color = "red", alpha = 0.2, adjust = 4) +
  geom_density(data = df.test, aes(x = item_price), color = "blue", alpha = 0.2, adjust = 4) +
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

#####################################################################################
# Data cleaning

clean.df <- df.train %>% 
  select(user_id, user_dob, user_reg_date, user_state, user_title,
         order_date, delivery_date, 
         item_id, brand_id, item_price) %>% 
  rename()

clean.df <- function(df) {
  require(dplyr)
  
  df <- df %>% select(-order_item_id)
  
  df$order_date    <- ymd(df$order_date)
  df$delivery_date <- ymd(df$delivery_date) # treat ? as missing
  df$user_dob      <- ymd(df$user_dob) # treat 1900-11-19 and ? as missing
  df$user_reg_date <- ymd(df$user_reg_date)
}

# convert: item_id, brand_id, user_id, user_title to factor
# convert: delivery_time = date.diff(delivery_date - order_date)


df.c <- clean.df(df.train)


#####################################################################################
# Model creation

# try a few candidate models

#####################################################################################
# Model evaluation

# ROC curve (after doing training / test set)
# 
# do cross-validation?