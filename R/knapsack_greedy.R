#' This is a function used to solve the knapsack problem with greedy algorithm
#'
#' This function solves the knapsack problem using a greedy algorithm. This does not
#' guarantee a perfect solution, but runs much faster than other options. In any case
#' the algorithm will return at least 50% of the original value
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
#' @importFrom utils tail
#' 
#' @examples 
#' n = 2000
#' knapsack_objects <-
#' data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#' greedy_knapsack(knapsack_objects[1:800,],3500)
#' greedy_knapsack(knapsack_objects[1:1200,],2000)
#' 
#' 
#' 
greedy_knapsack = function(x,W){
  if(W < 0){
    stop("The total weight cannot be negatie", call. = FALSE)
  }
  
  #setup
  x$value_per_weight = with(x,v/w) # calculating the value / weight ratio
  x$index = seq.int(nrow(x))
  ordered_x = x[order(-x$value_per_weight),]
  
  #solution 1
  total_weight = 0
  knapsack1 = ordered_x[0,]
  last_index = 0
  for (i in 1:nrow(ordered_x)) {
    total_weight = total_weight + ordered_x$w[i] 
    if(total_weight <= W){
      knapsack1[i,] = ordered_x[i,] # adding the items with the best v/w
      i = i +1
    } else {
      last_index = i
      break
    }
  }

  #solution 2
  total_weight = 0
  knapsack2 = ordered_x[0,]
  ordered_x = tail(ordered_x,-last_index) # removing the items added to the other knapsack
  for (j in 1:nrow(ordered_x)) {
    total_weight = total_weight + ordered_x$w[j]
    if(total_weight <= W){
      knapsack2[j,] = ordered_x[j,]
      j = j + 1
    } else {
      break
    }
  }
  
  #comparing solutions
  if((sum(knapsack1$v))>(sum(knapsack2$v))){
    a = list(sum(knapsack1$v),as.vector(knapsack1$index))
    names(a) = c("value","elements")
    return(a)
  } else {
    a = list(sum(knapsack2$v),as.vector(knapsack2$index))
    names(a) = c("value","elements")
    return(a)
    
  }
}
