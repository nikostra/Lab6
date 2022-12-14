% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_greedy.R
\name{greedy_knapsack}
\alias{greedy_knapsack}
\title{This is a function used to solve the knapsack problem with greedy algorithm}
\usage{
greedy_knapsack(x, W)
}
\arguments{
\item{x}{Data frame with all the elements that can be added to the knapsack.}

\item{W}{Weight limit of the knapsack.}
}
\value{
A list with the best computed value and the respective combination of
elements.
}
\description{
This function solves the knapsack problem using a greedy algorithm. This does not
guarantee a perfect solution, but runs much faster than other options. In any case
the algorithm will return at least 50% of the original value
The inputs are a data frame of items with a value and a weight and a parameter
determining the total weight limit
}
\examples{
n = 2000
knapsack_objects <-
data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
   v=runif(n = n, 0, 10000)
 )
greedy_knapsack(knapsack_objects[1:800,],3500)
greedy_knapsack(knapsack_objects[1:1200,],2000)

}
