% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{Dynamic programming for knapsack problem}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{is object that contains two vectors with weights and values. Must be a data frame with two colums w and v.}

\item{W}{is the maximum capacity(weight) of the knapsack and should be a positive numeric number.}
}
\value{
A list with two elements, maximum value and seleted items each.
}
\description{
If the weights are actually discrete values, we can use this to create an algorithm that can solve the knapsack problem exact by iterating over all possible values of w.
}
