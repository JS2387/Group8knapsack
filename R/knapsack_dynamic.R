#' @title knapsack_dynamic function
#' @description evaluates the solution for knapsack problem using dynamic programming
#' @returns list containing sum of values of chosen elements and the reference of the elements chosen in the final solution
#' @param x as a \code{data.frame} containing the weights and values of knapsack objects with column names 'w' & 'v' respectively
#' @param W as a \code{numeric value} defining the constraint of the knapsack capacity
#' @export
knapsack_dynamic <- function(x, W) {

  # initialize list to store list and vector to store chosen objects
  result_list <- list(value = 0, elements = c())
  chosen_elements <- c()

  ks_cap <- W                                       #assigning knapsack capacity
  weights <- x[,1]                                  #separate vector containing weights of objects
  weights <- c(0, weights)                          #adding 0 as first elements for looping
  values <- x[,2]                                   #separate vector containing the values of objects
  values <- c(0, values)                            #adding 0 as first element for looping
  n = length(weights)                               #number of objects for consideration
  ks_dp <- matrix(0, nrow = n, ncol = ks_cap+1)     #initializing result matrix of rows = number of elements & columns = knapsack capacity

  #creating the knapsack value calculation nested loop
  for (i in 2:n) {
    for (j in 2:(ks_cap+1)) {
      if (i == 1 || j == 1) ks_dp[i,j] = 0          #the first row and column of the matrix should be zero
      else if (weights[i] <= j) {
        ks_dp[i,j] = max(ks_dp[i-1,j], ks_dp[i-1,j-weights[i]]+values[i])                  #if object can fit in knapsack then take object if adding its value improves the value of the knapsack
        if (!(ks_dp[i,j] == ks_dp[i-1, j])) chosen_elements <- c(chosen_elements, i-1)     #if object was chosen then store its index in the result vector
      } else if (weights[i] > j) ks_dp[i,j] = ks_dp[i-1,j]                                 #if object weight was out of bounds then take the value of the previous row
    }
  }

  chosen_elements <- unique(chosen_elements)        #clean the chosen elements vector

  #assign solution to the resulting list
  result_list$value = ks_dp[n, ks_cap+1]
  result_list$elements <- chosen_elements

  return(result_list)
}
