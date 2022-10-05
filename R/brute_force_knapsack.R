#' This is a function used to solve the knapsack problem with a brute force algorithm
#'
#' This function solves the knapsack problem using a brute force algorithm.
#' The inputs are a data frame of items with a value and a weight and a parameter
#' determining the total weight limit
#'  
#' @param x Data frame with all the elements that can be added to the knapsack.
#' @param W Weight limit of the knapsack.
#' @param parallel Defines if the function should be run using parallel cores
#' 
#' @return A list with the best computed value and the respective combination of
#' elements.
#'  
#' @import parallel
#' @export
#' 
#' @examples 
#' n = 2000
#' knapsack_objects <-
#' data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#' brute_force_knapsack(knapsack_objects[1:12,],3500)
#' brute_force_knapsack(knapsack_objects[1:8,],2000)
brute_force_knapsack = function(x,W, parallel = FALSE){
  if(W < 0){
    stop("The total weight cannot be negative", call. = FALSE)
  }
  
  if (parallel == TRUE){
    n <- nrow(x)
    bfk_fun <- function(i, data, W, n){
      # Store combination number as a binary vector to get the active elements for each case
      raw_comb <- intToBits(i)
      # Save the binary vector to elements matrix
      elements <- c()
      for (j in 1:n){
        if (raw_comb[j] == 1){
          elements  <- append(elements, 1)
        }
        else{
          elements  <- append(elements, 0)
        }
      }
      val <- 0
      weight <- 0
      for (j in 1:n){
        if (elements[j] == 1){
          # Add the active elements weights and values
          weight <- weight + data$w[j]
          val <- val + data$v[j]
        }
      }
      if (weight <= W){
        # If weight is below the limit, save the value
        values <- val
      }
      return(values, elements)
    }
    # Get number of cores
    cores <- detectCores() -1
    # Set up the cluster
    c1 <- makeCluster(cores)
    clusterExport(c1, bfk_fun)
    clusterExport(c1, x)
    clusterExport(c1, W)
    clusterExport(c1, n)
    # Run the function in parallel
    res <- parSapply(c1, 1:2^n, function(i) bfk_fun(i, data = x, W, n))
    # Stop the cluster
    stopCluster(c1)
    return(res)
  }
  else{ # Original brute force function:
    n <- nrow(x)
    elements <- matrix(0, 2^n, n)
    values <- integer(2^n) # Number of possible combinations
    for (i in 1:length(values)){
      # Store combination number as a binary vector to get the active elements for each case
      raw_comb <- intToBits(i)
      # Save the binary vector to elements matrix
      for (j in 1:n){
        if (raw_comb[j] == 1){
          elements[i, j] <- 1
        }
        else{
          elements[i, j] <- 0
        }
      }
      val <- 0
      weight <- 0
      for (j in 1:n){
        if (elements[i,j] == 1){
          # Add the active elements weights and values
          weight <- weight + x$w[j]
          val <- val + x$v[j]
        }
      }
      if (weight <= W){
        # If weight is below the limit, save the value
        values[i] <- val
      }
    }
    # Get the maximum value:
    best_val <- max(values)
    # Get the active elements from the best case:
    el <- 1:n
    index <- which(values == best_val)
    el <- el[which(elements[index,] == 1)]
    
    res <- list("value" = best_val, "elements" = el)
    return(res)
  }
}
