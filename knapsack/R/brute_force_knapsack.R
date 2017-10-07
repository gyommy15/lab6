# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack <- function(x, W){
  
  #Check inputs are correct
  stopifnot(is.data.frame(x) & is.numeric(W) & W>0)
  stopifnot(colnames(x)==c("w", "v"))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(x$w>0 & x$v>0)
  
  num_of_items <- nrow(x)
  num_of_cases <- 2^nrow(x)
  
  #Generating weights and values with length equals to num_of_cases (initial value NA)
  weights <- rep(NA, num_of_cases)
  values <- rep(NA, num_of_cases)
  
  #Generating a list with num_of_cases elements (initial value 0)
  cases <- as.list(numeric(num_of_cases))
  
  
  for(i in 1:num_of_cases){
    #Binary coding for each case
    cases[[i]] <- as.numeric(intToBits(i)[1:(num_of_items)])
    
    #Making a data.frame with items binary code equals 1
    items_info <- x[cases[[i]] == 1,]
    
    #weights and Values sum for each case
    weights[i] <- sum(items_info[,1])
    values[i] <- sum(items_info[,2])
  }
  
  #All cases are checked.
  
  #Selecting the best case.
  weight_check <- which(weights<=W)
  value_check <- values[weight_check]
  best_case <- which.max(value_check)
  
  #Setting up the return using weight_check index.
  value <-values[weight_check[best_case]]
  elements <- (1:num_of_items)[cases[weight_check[best_case]][[1]]==1]
  
  return(list(value=value, elements=elements))
}
