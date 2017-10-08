#' Greedy heuristic approach for knapsack problem
#'
#' This algorithm will not give an exact result (but it can be shown that it will return at least 50% of the true maximum value), but it will reduce the computational complexity considerably (actually to O(n log n) due to the sorting part of the algorithm).
#' @param x is object that contains two vectors with weights and values. Must be a data frame with two colums w and v.
#' @param W is the maximum capacity(weight) of the knapsack and should be a positive numeric number.
#' @return A list with two elements, maximum value and seleted items each.
#' @export

# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

greedy_knapsack <- function(x,W){

  #Check inputs are correct
  stopifnot(is.data.frame(x) & is.numeric(W) & W>0)
  stopifnot(colnames(x)==c("w", "v"))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(x$w>0 & x$v>0)

  elements <- c()
  weights <- x$w
  values <- x$v
  val_per_wei <- values/weights
  ord <- order(val_per_wei, decreasing = TRUE)
  
  weight_sum <- 0
  value_sum <- 0

  for(i in ord){
    if(weights[i] + weight_sum <= W){
      weight_sum <- weight_sum + weights[i]
      value_sum <- value_sum + values[i]
      elements <- c(elements, i)
    }
    else if(weights[i] + weight_sum > W){
      break
    }
  }
  
  return(list(value=value_sum, elements=elements))
  
}