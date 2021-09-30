#' @title knapsack problem data set
#' @description Creates a data frame that can be used for implementing the knapsack algorithm
#' @param n as integer to represent the number of objects in the knapsack problem to be generated
#' @example knapsack_object(2000)
#' @export
knapsack_objects <- function(n) {
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}
