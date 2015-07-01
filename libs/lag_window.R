# Copyright (c) 2015 Farzad Noorian and Richard I.A. Davis.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################
# Implementation of Lag Windows. Usefull for Time-series forecasting

library("xts")

lag.matrix.numeric <- function(x, lag.indices, pstr=deparse(substitute(x))) {
  # collects and embeds lagged windows of data as a row vector in a matrix
  # only handles numeric vector types
  # INPUTS:
  #    x: data input (can handle multi-column data, such as data.frames)
  #    lag.index: index of lags to embed
  #    pstr: name of datacolumn (only used if x is a single column or vector)
  # OUTPUT:
  #    a matrix of lagged windows
  
  x = as.numeric(x)
  len_x = length(x)

  # check if lags fall out of the number of samples
  if (len_x <= max(lag.indices)) {
      stop("Lag window: Not enough data for the given Lag.")
  }
  stopifnot(length(lag.indices) >= 1)
    
  # put each lag into a matrix column
  embeded.ts = matrix(NA, nrow = len_x, ncol = length(lag.indices))
  for (i in seq_along(lag.indices)) {
    L = lag.indices[i]
    if (L >= 0) { # lags
      embeded.ts[(1+L):len_x, i] = x[1:(len_x - L)]
    } else if (L < 0) { # leads
      L = abs(L)
      embeded.ts[1:(len_x - L), i] = x[(1+L):len_x]
    }
  }
  
  # give each column a name
  colnames(embeded.ts) = ifelse(lag.indices < 0, 
                                paste0(pstr, ".lf", -lag.indices), # the LEAD FORWARD!
                                paste0(pstr, ".l", lag.indices)) # the lag
  
  return (embeded.ts)
}

lag.matrix.ts <- function(x, lag.indices, pstr=deparse(substitute(x))) {
  # collects and embeds lagged windows of data as a row vector in a matrix
  # handles ts and xts as well as numeric vector types
  # INPUTS:
  #    x: data input (can handle multi-column data, such as data.frames)
  #    lag.indices: index of lags to embed
  #    pstr: name of datacolumn (only used if x is a single column or vector)
  # OUTPUT:
  #    a matrix of lagged windows
  
  w = lag.matrix.numeric(x, lag.indices, pstr)
  
  # convert back to xts or ts if required
  if (is.ts(x)) {
    w = ts(w, start=start(x), end=end(x), frequency=frequency(x), deltat = deltat(x))
  } else if (is.xts(x)) {
    ind_range = index(x)
    w = xts(w, ind_range)
  }
  
  return(w)
}

lag.matrix.multi.column.ts <- function(x, lag.indices, pstr=deparse(substitute(x))) {
  # collects and embeds lagged windows of data as a row vector in a matrix
  # handles multicolumn ts and xts such as data.frames
  # INPUTS:
  #    x: data input (can handle multi-column data, such as data.frames)
  #    lag.indices: index of lags to embed
  #    pstr: name of datacolumn (only used if x is a single column or vector)
  # OUTPUT:
  #    a matrix of lagged windows
  
  if (length(lag.indices) == 0) { # return empty if no indices given
    return (NULL)
  }

  if ((is.null(dim(x))) || (ncol(x) == 1)) { # for one column objects
    return (lag.matrix.ts(x, lag.indices, pstr))
  } else { # for multi column objects
    # call lag.window each separately
    nc = ncol(x)
    hw = lapply(1:nc, function(i) lag.matrix.multi.column.ts(x[,i], lag.indices, pstr=paste0(colnames(x)[i])))
    
    # combine them
    hw.combined = do.call(cbind, hw)
    colnames(hw.combined) = as.vector(sapply(hw, colnames))
    
    return(hw.combined)
  }
}


# LaggedWindows <- function(x, p = 0, P = 0, freq = 1, shift = 0, pstr=deparse(substitute(x))) {  
#   # collects and embeds p lengthed windows of data as a vector
#   # INPUTS:
#   #    x: data input (can handle multi-column data, such as data.frames)
#   #    p: window length
#   #    P: seasonal window length
#   #    shift: if the time-series is to be shifted
#   #    freq: seasonal frequency
#   #    pstr: name of datacolumn
#   # OUTPUT:
#   #    a matrix of lagged windows
# 
#   n <- length(x)
# 
#   # the normal window order
#   ind.lag <- 0:p # it starts from 0!
#   
#   # seasonal window order
#   if (P * freq > 0) {
#     ind.lag = unique(c(ind.lag, 1:P * freq ))
#   }
#   
#   #  the lags
#   ind.lag = ind.lag + shift
#   
#   return (lag.matrix.multi.column.ts(x, ind.lag, pstr))
# }

LaggedWindows <- function(x, p = 0, P = 0, freq = 1, shift = 0, no.peeking = TRUE, pstr=deparse(substitute(x))) {  
  # collects and embeds p lengthed windows of data as a vector
  # INPUTS:
  #    x: data input (can handle multi-column data, such as data.frames)
  #    p: window length
  #    P: seasonal window length
  #    shift: if the time-series is to be shifted
  #    freq: seasonal frequency
  #    no.peeking: if TRUE, Lags < 0 (from future) are removed
  #    pstr: name of datacolumn
  # OUTPUT:
  #    a matrix of lagged windows

  n <- length(x)

  # the normal window order
  ind.lag <- 0:p # it starts from 0!
  
  # seasonal window order
  if (P * freq > 0) {
    ind.lag = unique(c(ind.lag, 1:P * freq ))
  }
  
  #  the lags
  ind.lag = ind.lag + shift
  
  # check peeking
  if (no.peeking) {
    ind.lag = ind.lag[ind.lag > 0]
  }
  
  return (lag.matrix.multi.column.ts(x, ind.lag, pstr))
}

CenteredWindows <- function(x, p = 1, pstr=deparse(substitute(x))) {  
  # Centers the lag winow around Lag 0, with width p on each side.
  #    x: data input (can handle multi-column data, such as data.frames)
  #    p: window length around lag 0
  #       p == 0 -> x itself
  #       p < 0  -> no data is returned
  
  if (p < 0) {
    return (NULL)
  }
  
  if (p == 0) {
    colnames(x) = paste0(pstr, ".l0")
    return(x)
  }
  
  return (LaggedWindows(x, 2*p+1, shift = -p, pstr=pstr, no.peeking = FALSE))
}

SeasonalWindows <- function(x, P = 1, freq = frequency(x), width = 0, shift = 0, no.peeking = TRUE, pstr=deparse(substitute(x))) {  
  # Creates a window around each seasonal lag, with width+shift/width-shift on each side.
  #    x: data input (can handle multi-column data, such as data.frames)
  #    P: seasonal window length
  #    freq: seasonal frequency
  #    width: width of the left or right window
  #    shift: shifts each window towards the end of time-series
  #    no.peeking: if TRUE, Lags < 0 (from future) are removed
  #    pstr: name of datacolumn
  
  # Sanity checks
  if (width < 0 ||
      P*freq <= 0) {
    return (NULL)
  }
  
  # get the lags
  ind.lag = unique(1:P * freq)
  
  # add the window width  
  left_windw = shift - width
  right_windw = shift + width
  
  shifted.lags = NULL
  for (i in left_windw:right_windw) {
    shifted.lags = unique(c(shifted.lags, i+ind.lag))
  }
  
  # check peeking
  if (no.peeking) {
    shifted.lags = shifted.lags[shifted.lags > 0]
  }

  # sort lags and extract data
  shifted.lags = sort(shifted.lags)
  
  return (lag.matrix.multi.column.ts(x, shifted.lags, pstr))
}
