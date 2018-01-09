################################################################################
# BADS-Model-NN.R
# 
# Phi Nguyen: phi.nguyen@outlook.com
#
################################################################################
# Description:
# Build model using a Neural Network approach
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/BADS_Project")
setwd(wd)

# Load packages
needs(caret, splines, mlr, xgboost, nnet, ModelMetrics, 
      tidyverse, lubridate, ggplot2)

source("Scripts/BADS.R")
source("Scripts/Helpful.R")

################################################################################
# MODEL CREATION (first pass)

