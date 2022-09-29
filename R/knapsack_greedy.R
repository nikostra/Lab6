greedy_knapsack = function(x,W){
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
