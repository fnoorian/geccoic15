factorizer1 <- function(x) {
  round(x * 2, -1) / 2
}

factorizer4 <- function(x) {
  ifelse(x == 0, 0,
         ifelse(x == 33, 33,
                round(x * 2, -1) / 2))
}