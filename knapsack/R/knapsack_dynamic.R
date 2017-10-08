#' Dynamic programming for knapsack problem
#'
#' If the weights are actually discrete values, we can use this to create an algorithm that can solve the knapsack problem exact by iterating over all possible values of w.
#' @param x is object that contains two vectors with weights and values. Must be a data frame with two colums w and v.
#' @param W is the maximum capacity(weight) of the knapsack and should be a positive numeric number.
#' @return A list with two elements, maximum value and seleted items each.
#' @export
#' 
# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# knapsack_dynamic(x= knapsack_objects[1:8,], W = 3500)

knapsack_dynamic <- function(x,W){
  
  #Check inputs are correct
  stopifnot(is.data.frame(x) & is.numeric(W) & W>0)
  stopifnot(colnames(x)==c("w", "v"))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(x$w>0 & x$v>0)
  
  best_elements <- c()
  num_of_items <- nrow(x)
  weights <- x$w
  values <- x$v
  m <- matrix(nrow = (num_of_items+1), ncol = W+1)
  
  for(i in 1:(W+1)){
    m[1, i] <- 0
  }
  
  for(i in 1:num_of_items){
    for(j in 1:(W+1)){
      if(weights[i] > j){
        m[i+1,j] <- m[i,j]
      }
      else{
        m[i+1,j] <- max(m[i,j],m[i,j-weights[i]]+values[i])
      }
    }
  }

  #Confirm best elements by matrix m
  i <- nrow(m)
  j <- ncol(m)
  while(i > 1 ){
    
    if( m[i-1,j] < m[i,j] ){
      best_elements <- c(best_elements, i-1)
      i <- i - 1
      j <- j - weights[i]
      
    }else{
      i <- i - 1
    }
  }
  return(list(value=max(m), elements=sort(best_elements)))
}