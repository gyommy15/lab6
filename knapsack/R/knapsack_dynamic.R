knapsack_dynamic <- function(x,W){
  
  #Check inputs are correct
  stopifnot(is.data.frame(x) & is.numeric(W) & W>0)
  stopifnot(colnames(x)==c("w", "v"))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(x$w>0 & x$v>0)
  
  
  
}