# Lab6
<!-- badges: start -->
  [![R-CMD-check](https://github.com/nikostra/Lab6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nikostra/Lab6/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
  ### Running times (no optimization)
  Brute force (n=16): 29.6s
  
  Dynamic programming (n=500): 2.22s
  
  Greedy algorithm (n=1000000): 286ms
  
  ### Profiling insights:
  In brute_force_knapsack() the rbind() function takes up the most time
