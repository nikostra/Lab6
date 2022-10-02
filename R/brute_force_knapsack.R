#' This is a function used to solve the knapsack problem with brute force algorithm
#'
#' This function solves the knapsack problem using a brute force algorithm.
#' The inputs are a data frame of items with a value and a weight and a parameter
#' determining the total weight limit
#'  
#' @param x Data frame with all the elements that can be added to the knapsack.
#' @param W Weight limit of the knapsack.
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
    n <- nrow(x)
    elements <- c()
    values <- integer(2^n) # Number of possible combinations
    for (i in 1:length(values)){
      # Store combination number as a binary vector to get the active elements for each case
      elements <- rbind(elements, intToBits(i)[1:n]) 
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
