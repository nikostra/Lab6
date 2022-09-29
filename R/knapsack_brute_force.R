#for testing:
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

# test with brute_force_knapsack(knapsack_objects[1:12,],3500)
#recursive solution
brute_force_knapsack = function(x,W){

  knapsack_recursive = function(x,W,n){

    #end case of recursion
    if (n == 0 || W == 0){
      return(0)
    }
    #check if item is too big
    if (x$w[n] > W){
      return(knapsack_recursive(x,W, n -1))
    } else {
      v1 = c()
      # if item is improving max value, use it, otherwise ignore it
      a = x$v[n] + knapsack_recursive(x, W - x$w[n], n - 1)
      b = knapsack_recursive(x,W, n - 1)
      if(a > b){
        return(a)
      } else {
        return(b)
      }
      #return(max(x$v[n] + knapsack_recursive(x, W - x$w[n], n - 1), knapsack_recursive(x,W, n - 1)))
    }
  }
  return(knapsack_recursive(x,W,nrow(x)))
}
