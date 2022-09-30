#' This is a function used to solve the knapsack problem with brute force algorithm
#'
#' This function solves the knapsack problem using a recursive brute force algorithm.
#' The inputs are a data frame of items with a value and a weight and a parameter
#' determining the total weight limit
#'  
#' @param x Data frame with all the elements that can be added to the knapsack.
#' @param W Weight limit of the knapsack.
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
#' brute_force_knapsack(knapsack_objects[1:12,],3500)
#' brute_force_knapsack(knapsack_objects[1:8,],2000)
#' 
#' 

#for testing:
# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

# test with brute_force_knapsack(knapsack_objects[1:12,],3500)
brute_force_knapsack = function(x,W){
  if(W < 0){
    stop("The total weight cannot be negatie", call. = FALSE)
  }
  max_value = 0
  max_elements = c()
  
  #add matrix with all results and pick best one at the end
  knapsack_recursive = function(x,W,n,vec,value){

    #end case of recursion
    if (n == 0 || W == 0){
      if(value > max_value){
        #if the value is a new top value, save the current elements and update top value
        max_elements <<- vec
        max_value <<- value
      }
      return(0)
    }
    #check if item is too big
    if (x$w[n] > W){
      return(knapsack_recursive(x,W, n -1,vec,value))
    } else {
      v1 = c(vec,n) # add current index to the vector
      # if item is improving max value, use it, otherwise ignore it
      a = x$v[n] + knapsack_recursive(x, W - x$w[n], n - 1,v1,(value + x$v[n]))
      b = knapsack_recursive(x,W, n - 1,vec,value)
      if(a > b){
        return(a)
      } else {
        return(b)
      }
    }
  }
  best_value = knapsack_recursive(x,W,nrow(x),c(),0)
  max_elements = sort(max_elements)
  a = list(max_value,max_elements)
  names(a) = c("value","elements")
  return(a)
}
