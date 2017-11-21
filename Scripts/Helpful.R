################################################################################
# Helpful.R
#
################################################################################
# Description:
# Helpful functions designed to aid in BADS project. Mostly for data 
# exploration.
#
# dist.check() - compares distribution of cat variables in training & test sets
# return.check() - looks at return rates by category in training set
# num.check() - looks at return rates for a numerical variable in training set 
# discrete.bins() - organizes observations into selected # of bins
# discrete.power() - organizes observations into bins (bins organized in size 
# by power)
# 
################################################################################

# check distribution of categorical variables (and how they might differ)
dist.check <- function(df.train, var) {
  if (var == "brand_id") {df.train[[var]] <- df.train[[var]] + 100}
  dist.train <- data.frame(
    train = table(df.train[[var]])[order(table(df.train[[var]]))]
  )
  dist.test <- data.frame(
    test = table(df.test[[var]])[order(table(df.test[[var]]))]
  )
  
  dister   <- dist.train %>% 
    full_join(dist.test, by = c("train.Var1" = "test.Var1"))
  
  dister[is.na(dister)]    <- 0
  dister$Difference        <- 2 * dister[, 3] / dister[, 2] # should be near 1
  
  names(dister$train.Var1) <- "Variable"
  return(dister)
}

# check return rates of cat variables
return.check <- function(df, var) {
  return.table <- as.data.frame(as.matrix.data.frame(
    table(df[[var]], df$return)
  ))
  return.table[[var]] <- levels(df[[var]])
  return.table <- return.table %>% 
    select(var, V1, V2) %>% 
    rename(Keep = V1, Return = V2) %>% 
    mutate(Total = Keep + Return, ReturnRate = round(Return / Total, 3))
  
  return(return.table)
}

# check return rates of numeric variables
num.check <- function(df, var) {
  prices.df <- df %>% arrange(get(var))
  tables    <- as.data.frame(table(prices.df[[var]], prices.df$return))
  tables    <- tidyr::spread(tables, Var2, Freq)
  tables    <- tables %>% 
    rename(Keep = '0', Return = '1') %>% 
    mutate(Total      = Keep + Return, 
           ReturnRate = round(Return / Total, 3),
           Var1       = as.numeric(levels(Var1))[Var1])
  
  return(tables)
}

# Function creates discrete buckets based on chosen variable
# Note that fxn will try to create numbins selected, but if some observations 
# span multiple bins, there may be fewer bins
discrete.bin <- function(df, variable, numbins = 10) {
  df <- df %>% 
    arrange(df[[variable]]) %>% 
    mutate(allsums = cumsum(Total))
    
  cutoff  <- round(tail(df, 1)$allsums / numbins)
  binsmax <- as.integer(seq(cutoff, tail(df, 1)$allsums, by = cutoff))
  if (length(binsmax) < numbins) {binsmax <- c(binsmax, tail(df, 1)$allsums)}
  
  # last value underbins
  binidx  <- sapply(binsmax, function(x) last(which(df$allsums <= x))) 
  
  maxval <- df[[variable]][binidx]
  maxval <- c(0, maxval)
  
  df$bins  <- paste0("[0, ", maxval[2], "]")
  
  for (i in 2:length(maxval)) {
    for (j in 1:nrow(df)) {
      if (df[[variable]][j] > maxval[i]) {
        df$bins[j] <- paste0("(", maxval[i], ", ", maxval[i+1], "]")
      }
    }
  }
  
  df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
    group_by(bins) %>% 
    summarize(ReturnRate = sum(Return) / sum(Total))
  
  return(df.bins)
}

# create a discrete function that follows power
discrete.power <- function(df, variable, numbins = 10, powerval = 5) {
  df <- df %>% 
    arrange(df[[variable]])
  
  cutoffs <- powerval ^ (1:numbins)
  if(max(cutoffs) > max(df[[variable]])){
    message("Bin values exceeds total items in group. Truncating # of bins.")
   cutoffs <- cutoffs[cutoffs < max(df[[variable]])] 
  }
  groupings    <- c(0, cutoffs, max(df[[variable]]))
  
  df$bins  <- paste0("[0, ", groupings[2], "]")
  
  for (i in 2:length(groupings)) {
    for (j in 1:nrow(df)) {
      if (df[[variable]][j] > groupings[i]) {
        df$bins[j] <- paste0("(", groupings[i], ", ", groupings[i+1], "]")
      }
    }
  }
  
  df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
    group_by(bins) %>% 
    summarize(ReturnRate = sum(Return) / sum(Total))
  
  return(df.bins)
}