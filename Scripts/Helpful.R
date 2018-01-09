################################################################################
# Helpful.R
#
# Phi Nguyen: phi.nguyen@outlook.com
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
# assign.bins() - creates bins based off bins created in discrete.bins()
# samplefxn() - imputation fxn (misnomer)
# 
# build.glm() - builds predictions and classification table for glm model
# log.loss() - calculates log loss error
# brier.score() - calculates brier score
# 
################################################################################

# check distribution of categorical variables (and how they might differ)
dist.check <- function(df.train, var) {
  if (var == "brand_id") df.test[[var]] <- df.test[[var]] - 100
  if (var == "item_id") df.test[[var]] <- (df.test[[var]] / 2) - 1
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

# Create buckets in dataset
assign.bins <- function(df, buckets, variable) {
  start    <- unlist(gregexpr(buckets$bins, pattern = ", "))
  end      <- unlist(gregexpr(buckets$bins, pattern = "]"))
  ceilings <- c(0, as.numeric(substr(buckets$bins, start + 2, end - 1)))
  
  # set arbitrarily large ceiling
  ceilings[length(ceilings)] <- 99999
  
  grouping <- cut(df[[variable]], ceilings, include.lowest = TRUE)
  return(grouping)
}

# imputation
samplefxn <- function(df, var, type) {
  idx <- is.na(df[[var]])
  len <- sum(idx)
  
  values <- switch(type,
                   sample = sample(df[!idx, var], size = len, replace = TRUE),
                   mean   = rep(round(mean(df[[var]], na.rm = TRUE)), 
                                times = len))
  
  return(values)
}

# build glm model
build.glm <- function(mod, trainset, testset, alpha) { 
  matrix.x  <- model.matrix(mod, data = trainset)
  mod1      <- cv.glmnet(x = matrix.x, y = trainset$return, alpha = alpha, 
                         family = "binomial", standardize = TRUE)
  # plot(mod1)
  # mod1$lambda.1se
  coefff = coef(mod1, s = "lambda.1se")
  
  # Make prediction
  new.x <- model.matrix(mod, data = testset)
  pred  <- predict(mod1, newx = new.x, s = "lambda.1se", type = "response")
  
  test  <- data.frame(pred, testset$return, round(pred))
  names(test) <- c("prob", "actual", "result")
  
  # Check performance
  check1 <- table(predicted = test[, 3], actual = testset$return)
  mce1   <- 1 - sum(diag(check1)) / sum(check1)
  
  # GET FPR / FNR
  test$Class <- with(data = test, ifelse(
    actual == result & actual == 1, "TP", ifelse(
      actual == result & actual == 0, "TN", ifelse(
        actual != result & actual == 1, "FN", "FP")
      )
    ) 
  )
  
  test <- test %>% 
    bind_cols(testset) %>% 
    select(prob, actual, result, Class)
  
  FPR <- test %>% 
    filter(actual == 0) %>% 
    summarize(FPR = sum(result) / n()) %>% 
    as.numeric()
  
  FNR <- test %>% 
    filter(actual == 1) %>% 
    summarize(FNR = (n() - sum(result)) / n()) %>% 
    as.numeric()
  
  return(list(mod = mod1, Coef = coefff, Results = test,
              ClassTable = check1, FPR = FPR, FNR = FNR, MCE = mce1))
}

# ROCINFO: http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html#interpretation-and-reporting

log.loss <- function(act, pred) {
    eps  <- 1e-15
    nr   <- length(pred)
    pred <- matrix(sapply(pred, function(x) max(eps,x)), nrow = nr) 
    ll   <- sum(act*log(pred) + (1-act)*log(1-pred))
    ll   <-  ll * -1/(length(act)) 
    return(ll)
}

brier.score <- function(act, pred) {
    nr    <- length(pred)
    brier <- (1/nr) * sum((pred - act)^2)
    return(brier)
}
