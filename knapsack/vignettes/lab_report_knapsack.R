## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
#Basic setting
library(knapsack)

set.seed(42)
n <- 2000
knapsack_objects <-
 data.frame(
   w=sample(1:4000, size = n, replace = TRUE),
   v=runif(n = n, 0, 10000))

## ------------------------------------------------------------------------
system.time({brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)})

## ------------------------------------------------------------------------
system.time({knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)})

## ------------------------------------------------------------------------
set.seed(42)
n <- 1000000
knapsack_objects <-
 data.frame(
   w=sample(1:4000, size = n, replace = TRUE),
   v=runif(n = n, 0, 10000))

system.time({greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000)})

## ------------------------------------------------------------------------
require(lineprof)

lineprof(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))

## ------------------------------------------------------------------------
lineprof(kd <- knapsack_dynamic(x = knapsack_objects[1:20,], W = 3500))

## ------------------------------------------------------------------------
lineprof(gk <- greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

## ------------------------------------------------------------------------
system.time({brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)})

system.time({brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE)})

