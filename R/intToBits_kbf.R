
itb_brute_force_knapsack = function(x,W){
  n <- nrow(x)
  elements <- c()
  values <- integer(2^n) # Number of possible combinations
  for (i in 1:2^n){
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
