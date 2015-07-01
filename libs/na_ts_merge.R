na.ts.merge <- function(main, other) {
  # replaces NA's in main with the values in other
  # both must be xts
  all = merge(main, other)
  all[is.na(all[,1]),1] = all[is.na(all[,1]),2]
  
  return (all[,1])
}

na.ts.double.merge <- function(missing, pred_forward, pred_backward, verbose=FALSE) {
  # replaces NA's in main with the values in other
  # weights forward and backwards accordingly
  # all must be xts
  
  timeseries = missing
  
  while (any(is.na(timeseries))) {
    if (verbose) print(paste("Remaining NAs:", sum(is.na(timeseries))))
    
    first_na = which(is.na(timeseries))[1]
    na_len =  which(!is.na(timeseries[-(1:first_na)]))[1]
    
    before_ind = first_na
    after_ind = first_na + na_len - 1
    
    if (is.na(after_ind)) {
      stop()
    }
    
    time_before = format(index(timeseries[before_ind]), "%Y-%m-%d %H:%M:%S")
    time_after = format(index(timeseries[after_ind]), "%Y-%m-%d %H:%M:%S")
    time_missing = paste(time_before, time_after, sep = "/")
    
    p1 = pred_forward[time_missing]
    p2 = pred_backward[time_missing]
    
    # weight the forward and backward rescovery
    w = seq(0, 1, along.with = p1)
    #w = cumsum(abs(dx)/sum(abs(dx)))
    #w = cumsum((dx)^2/sum((dx)^2))
    replacement = (1-w) * p1 + w*p2 
    
    timeseries[time_missing] = replacement
  }
  
  return(timeseries)
}

