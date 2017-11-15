# Helpful functions

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
  dister$Difference        <- 2 * dister[, 3] / dister[, 2] # should be close to 1
  
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

# maybe discretize price to see effects
discret <- function(df, numbins = 10) {
  df <- df %>% 
    mutate(allsums = cumsum(Total))
  cutoff  <- round(tail(df, 1)$allsums / numbins)
  binsmax <- as.integer(seq(cutoff, tail(df, 1)$allsums, by = cutoff))
  if (length(binsmax) < numbins) {binsmax <- c(binsmax, tail(df, 1)$allsums)}
  
  # last value underbins
  binidx  <- sapply(binsmax, function(x) last(which(df$allsums <= x))) 
  
  pricemax <- df[,1][binidx]
  pricemax <- c(0, pricemax)
  
  df$bins  <- paste0("[0, ", pricemax[2], "]")
  
  for (i in 2:length(pricemax)) {
    for (j in 1:nrow(df)) {
      if (df[,1][j] > pricemax[i]) {
        df$bins[j] <- paste0("(", pricemax[i], ", ", pricemax[i+1], "]")
      }
    }
  }
  
  df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
    group_by(bins) %>% 
    summarize(ReturnRate = sum(Return) / sum(Total))
  
  return(df.bins)
}