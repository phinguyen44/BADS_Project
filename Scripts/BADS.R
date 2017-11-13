########################################################################################
# BADS.R
#
########################################################################################
# Description:
# BADS project
# 
########################################################################################

########################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(glmnet, caret, tidyverse, lubridate, zoo, corrplot)

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

########################################################################################
# Data exploration and cleaning

clean.df <- function(df) {
  df$order_date    <- as.Date(df$order_date, format = "%Y-%m-%d")
  df$delivery_date <- as.Date(df$delivery_date, format = "%Y-%m-%d") # treat ? as missing
  df$user_dob      <- as.Date(df$user_dob, format = "%Y-%m-%d") # treat 1900-11-19 and ? as missing
  df$user_reg_date <- as.Date(df$user_reg_date, format = "%Y-%m-%d")
}

# do date.diff delivery-date - order-date as var
# drop: order_item_id, item_color
# convert: item_id, brand_id, user_id, user_title to factor
# convert: delivery_time = date.diff(delivery_date - order_date)

df.c <- clean.df(df)

# compare distribution of variables 

########################################################################################
# Model evaluation

# ROC curve (after doing training / test set)
# 
# do cross-validation?