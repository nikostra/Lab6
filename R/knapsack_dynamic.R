#for testing:
# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

knapsack_dynamic <- function(x,W){
  n <- nrow(x)
  m <- matrix(0, n+1, W+1)
  e <- c()
  for (j in 1:(W+1)){
    m[1, j] <- 0
  }
  for (i in 1:(n+1)){
    m[i, 1] <- 0
  }
  for (i in 2:(n+1)){
    for (j in 2:(W + 1)){
      if (isTRUE(x$w[i] > j)){
        m[i, j] <- m[i-1, j]
      }
      else{
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
        if (isTRUE(!(m[i, j] == m[i-1, j]) && (!(i %in% e)))){
          e <- append(e, i)
        }
      }
    }
  }
  res <- list("value" = m[n, W], "elements" = e)
  return(res)
}

