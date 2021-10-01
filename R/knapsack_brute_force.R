#' @title knapsack_brute_force function
#' @description evaluates the solution for knapsack problem by calculating sum of weights & profits for each combination possible for given input
#' @returns list containing sum of values of chosen elements and the reference of the elements chosen in the final solution
#' @param x as a {\code}data.frame containing the weights and values of knapsack objects with column names 'w' & 'v' respectively
#' @param W as a {\code}numeric value defining the constraint of the knapsack capacity
#' @export
knapsack_brute_force <- function(x, W) {

  #check the validity of the inputs

  if(!(class(x) == "data.frame")) stop("data set is not a data frame")
  if(!(ncol(x) == 2) | !(colnames(x)[1] == "w") | !(colnames(x)[2] == "v")) stop("incompatible data frame type")
  if(any(x < 0)) stop("data frame contains negative values")

  #create list for storing output
  result_list <- list(value = 0, elements = c())

  n <- length(x$v) #to count number of combinations we will evaluate
  value_sum = 0

  for (i in 1:(2^n - 1)) {
    comb_bin <- as.integer(intToBits(i))
    comb_elements <- which(comb_bin > 0)
    sub_df <- subset(x, as.integer(row.names(x)) %in% comb_elements)
    if (sum(sub_df$w) < W && sum(sub_df$v) > value_sum) {
      value_sum = sum(sub_df$v)
      value = sum(sub_df$v)
      elements = comb_elements
    }
  }
  result_list$value = value
  result_list$elements = elements
  return(result_list)
}
