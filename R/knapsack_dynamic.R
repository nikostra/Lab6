#' This is a function used to solve the knapsack problem with the dynamic 
#' programming solution
#' 
#' This function solves the knapsack problem using dynamic programming.
#' The inputs are a data frame of items with a value and a weight and a parameter
#' determining the total weight limit
#'  
#' @param x Data frame with all the elements that can be added to the knapsack.
#' @param W Weight limit of the knapsack.
#' 
#' @return A list with the best computed value and the respective combination of
#' elements.
#'  
#' @export
#' 
#' @examples 
#' n = 2000
#' knapsack_objects <-
#' data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#' knapsack_dynamic(knapsack_objects[1:12,],3500)
#' knapsack_dynamic(knapsack_objects[1:8,],2000)

knapsack_dynamic <- function(x,W){
  # Extract number of elements:
  n <- nrow(x)
  # Initialize dynamic matrix:
  m <- matrix(0, n+1, W+1)
  # Start case matrix:
  cases <- matrix(0, 1, n+1)
  for (i in 2:(n+1)){
    for (j in 1:(W + 1)){
      # If current element weight is higher than the current loop weight, keep
      # the previous best value for such weight:
      if (isTRUE(x$w[i] > j)){
        m[i, j] <- m[i-1, j]
      }
      # If current element weight is lower than the current loop weight, save 
      # the maximum between the previous best value for this weight or the new
      # value computed by summing the current element value:
      else{
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
        # If a new value is saved in the dynamic matrix, append the new value
        # and combination of elements to the case matrix:
        if ((!(m[i, j] %in% cases)) && (i != n+1)){
          old_val <- m[i-1, j-x$w[i]]
          old_index <- match(old_val, cases)
          new_row <- c(m[i,j],cases[old_index, 2:(n+1)])
          new_row[i+1] <- 1
          cases <- rbind(cases, new_row)
        }
      }
    }
  }
  # Retrieve the best value and combination of elements from the case matrix:
  best_index <- match(m[n,W], cases[,1])
  best_comb <- cases[best_index,2:(n+1)]
  elements <- 1:n
  elements <- elements[which(best_comb == 1)]
  # Save the results in a list:
  res <- list("value" = m[n, W], "elements" = elements)
  return(res)
}

