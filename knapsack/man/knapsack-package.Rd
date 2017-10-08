\name{knapsack-package}
\alias{knapsack-package}
\alias{knapsack}
\docType{package}
\title{
\packageTitle{knapsack}
}
\description{
\packageDescription{knapsack}
}
\details{

brute_force_knapsack: The only solution that is guaranteed to give a correct answer in all situations for the knapsack problem is using brute-force search, i.e. going through all possible alternatives and return the maximum value found. 

knapsack_dynamic: If the weights are actually discrete values, we can use this to create an algorithm that can solve the knapsack problem exact by iterating over all possible values of w. 

greedy_knapsack: This algorithm will not give an exact result (but it can be shown that it will return at least 50\% of the true maximum value), but it will reduce the computational complexity considerably (actually to O(n log n) due to the sorting part of the algorithm).
 
}
\author{
\packageAuthor{knapsack}

Maintainer: \packageMaintainer{knapsack}
}
\references{
Dynamic Programming: \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem} 

Greedy Heuristic: \url{https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm}
}
\keyword{ package }