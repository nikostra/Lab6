# Lab6
<!-- badges: start -->
  [![R-CMD-check](https://github.com/nikostra/Lab6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nikostra/Lab6/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
  ### Running times (no optimization)
  Brute force (n=16): 29.6s
  
  Dynamic programming (n=500): 2.22s
  
  Greedy algorithm (n=1000000): 286ms
  
  ### Profiling insights:
  In brute_force_knapsack() the rbind() function takes up the most time. The function was modified to intialize the element matrix and change each row instead of appending each row with rbind().

  After optimization the runtime decreased significantly to 1.16s for n=16.
  
  Additionally we implemented an alternative brute force algorithm (recursive_alg_knapsack() ) that uses recursion, which is also significantly faster than the default brute force algorithm. For n=16 it completes the computation in 902µs.
