diff_accumulator <- function(timeseries.with.na, diff_pred, verbose=FALSE) {
  # Fits a "diff" forecast into a NA gap of a time-series
  # It first accumulates the diff forwards and backwards, and then weights it
  # such that end tail of forwards and head of backwards are merged.
  
  timeseries = timeseries.with.na
  
  while (any(is.na(timeseries))) {
    if (verbose) print(paste("Remaining NAs:", sum(is.na(timeseries))))
    
    first_na = which(is.na(timeseries))[1]
    na_len =  which(!is.na(timeseries[-(1:first_na)]))[1]
    
    before_ind = first_na - 1
    after_ind = first_na + na_len
    
    if (is.na(after_ind)) {
      stop()
    }

    time_before = format(index(timeseries[before_ind]), "%Y-%m-%d %H:%M:%S")
    time_after = format(index(timeseries[after_ind]), "%Y-%m-%d %H:%M:%S")
    
    x0 = timeseries[time_before]
    xn = timeseries[time_after]
    
    dx = diff(timeseries)[paste(time_before, time_after, sep = "/")]
    #dx = dx[1:(na_len+2)] # due to a bug in the / when time is at day boundary
    dx[is.na(dx)] = diff_pred[index(dx[is.na(dx)])]

    
    # compute forward and backward recovery
    d_forward = head(dx[-1], -1)
    d_backward = rev(head(lag(dx, -1)[-1], -1))
    x1 = c(x0, as.numeric(x0) + cumsum(d_forward), xn)
    x2 = c(x0, as.numeric(xn) + (-rev(cumsum(d_backward))), xn)
    
    # weight the forward and backward rescovery
    w = seq(0, 1, along.with = x1)
    #w = cumsum(abs(dx)/sum(abs(dx)))
    #w = cumsum((dx)^2/sum((dx)^2))
    x_r = (1-w) * x1 + w*x2 
    
    timeseries[(before_ind+1):(after_ind-1)]  = head(x_r[-1], -1)
  }
  
  return(timeseries)
}

