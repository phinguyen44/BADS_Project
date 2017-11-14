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
      tidyverse, lubridate, zoo, ggplot2, 
      corrplot, PerformanceAnalytics)

# Read data
df.train <- read.csv("Data/BADS_WS1718_known.csv")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")

#####################################################################################
# Data exploration

## TODO: MAKE SURE FACTORS ARE ORDERED CORRECTLY !!! !!! !!! 

# check distribution of categorical variables (and how they might differ)
dist.check <- function(var) {
  dist.train <- data.frame(
    train = table(df.train[[var]])[order(table(df.train[[var]]))]
  )
  dist.test <- data.frame(
    test = table(df.test[[var]])[order(table(df.test[[var]]))]
  )
  dister   <- dist.train %>% 
    full_join(dist.test, by = c("train.Var1" = "test.Var1"))
  
  dister[is.na(dister)]    <- 0
  # dister[, 2]              <- round(dister[, 2]/sum(dister[, 2]), 3)
  # dister[, 3]              <- round(dister[, 3]/sum(dister[, 3]), 3)
  dister$Difference        <- dister[, 3] - dister[, 2]
  
  names(dister$train.Var1) <- "Variable"
  return(dister)
}

# check return rates of cat variables
return.check <- function(var) {
  return.table <- as.data.frame(as.matrix.data.frame(
    table(df.train[[var]], df.train$return)
  ))
  rownames(return.table) <- levels(df.train[[var]])
  colnames(return.table) <- c("Keep", "Return")
  return.table$Total     <- with(return.table, Return + Keep)
  return.table$perc.ret  <- round(with(return.table, (Return / (Return + Keep))), 2)
  return(return.table)
}

fxns <- list(dist.check = dist.check, return.check = return.check)

item_color <- lapply(fxns, function(f) f("item_color"))
item_size  <- lapply(fxns, function(f) f("item_size"))
user_title <- lapply(fxns, function(f) f("user_title"))
user_state <- lapply(fxns, function(f) f("user_state"))
item_id    <- lapply(fxns, function(f) f("item_id"))
user_id    <- lapply(fxns, function(f) f("user_id"))

# check distribution (numeric): item_price

#####################################################################################
# Data cleaning

clean.df <- function(df) {
  df$order_date    <- ymd(df$order_date)
  df$delivery_date <- ymd(df$delivery_date) # treat ? as missing
  df$user_dob      <- ymd(df$user_dob) # treat 1900-11-19 and ? as missing
  df$user_reg_date <- ymd(df$user_reg_date)
}

# drop: order_item_id, item_color
# convert: item_id, brand_id, user_id, user_title to factor
# convert: delivery_time = date.diff(delivery_date - order_date)


df.c <- clean.df(df.train)

# corrplots of variables

#####################################################################################
# Model creation

# try a few candidate models

#####################################################################################
# Model evaluation

# ROC curve (after doing training / test set)
# 
# do cross-validation?