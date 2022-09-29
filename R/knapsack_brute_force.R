#for testing:
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#recursive solution
brute_force_knapsack = function(x,W){
  v = c()

  knapsack_recursive = function(x,W,n,v){
    v1 = c()
    v2 = c()
    
    #end case of recursion
    if (n == 0 || W == 0){
      return(0)
    }
    #check if item is too big
    if (x$w[n] > W){
      return(knapsack_recursive(x,W, n -1,v2))
    } else {
      v1 = c(v1,n)
      # if item is improving max value, use it, otherwise ignore it
      a = x$v[n] + knapsack_recursive(x, W - x$w[n], n - 1,v1)
      b = knapsack_recursive(x,W, n - 1,v2)
      if(a > b){
        v <- v1
        return(a)
      } else {
        v <- v2
        return(b)
      }
      #return(max(x$v[n] + knapsack_recursive(x, W - x$w[n], n - 1), knapsack_recursive(x,W, n - 1)))
    }
  }
  print(v)
  return(knapsack_recursive(x,W,nrow(x)))
}
