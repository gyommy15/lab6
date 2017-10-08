#' Brute force search for knapsack problem
#'
#' The only solution that is guaranteed to give a correct answer in all situations for the knapsack problem is using brute-force search, i.e. going through all possible alternatives and return the maximum value found.
#' @param x is object that contains two vectors with weights and values. Must be a data frame with two colums w and v.
#' @param W is the maximum capacity(weight) of the knapsack and should be a positive numeric number.
#' @param parallel is the logical 
#' @return A list with two elements, maximum value and seleted items each.
#' @export
#' @importFrom utils combn
#' @import parallel lineprof

# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack <- function(x, W, parallel = FALSE){
  
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
  
  if(parallel == FALSE){
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
  
  }else{
    
    requireNamespace("parallel")
    
    #Getting cores // set to 2 to observe cran policy
    cores <- parallel::detectCores()-2
    
    #Making cluster
    cl <- makeCluster(cores)
    
    #Applying parLapplyLB
    clusterExport(cl, c("x"), envir = environment())
    cases_list <- parLapplyLB(cl, 1:num_of_items, fun =  function(y) {
      combn(rownames(x), y, paste0, collapse = " ")
      
    })
    weights_list <- parLapplyLB(cl, 1:num_of_items, fun =  function(y) {
      combn(x$w, y, sum)
      
    })
    values_list <- parLapplyLB(cl,1:num_of_items, fun =  function(y) { 
      combn(x$v, y , sum)
      
    })
    
    stopCluster(cl)
    
    #Unlisting
    cases_unlist <- unlist(cases_list)
    weights_unlist <- unlist(weights_list)
    values_unlist <- unlist(values_list)
    
    #Finding maximum value
    value <- max(values_unlist[which(weights_unlist <= W)])
    
    #Finding best case
    elements <- cases_unlist[which(values_unlist == value)]

    return(list(value = value, elements = as.numeric(strsplit(elements, " ")[[1]])))
  }
}