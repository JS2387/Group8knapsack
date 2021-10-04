knapsack_dynamic <- function(x, W) {

  result_list <- list(value = 0, elements = c())
  chosen_elements <- c()

  ks_cap <- W
  weights <- x[,1]
  weights <- c(0, weights)
  values <- x[,2]
  values <- c(0, values)
  n = length(weights)
  ks_dp <- matrix(0, nrow = n, ncol = ks_cap+1)
  for (i in 2:n) {
    for (j in 2:(ks_cap+1)) {
      if (i == 1 || j == 1) ks_dp[i,j] = 0
      else if (weights[i] <= j) ks_dp[i,j] = max(ks_dp[i-1,j], ks_dp[i-1,j-weights[i]]+values[i])
      else if (weights[i] > j) ks_dp[i,j] = ks_dp[i-1,j]
    }
  }

  result_list$value = ks_dp[n, ks_cap+1]

    i = n+1
    j = cap_left+1
    k=1

    while (i>1 && j>1) {
      if (!(calc_matrix[i,j] == calc_matrix[i-1,j])) {
        chosen_elements[k] <- i-1
        j = j - weight_vec[i]
        i = i-1
        k=k+1
      } else {i <- i - 1}
    }

    result_list$elements <- chosen_elements

    return(result_list)
  }

  result_list <- ks_dp(n, cap_left, weight_vec, value_vec)




}

write.csv(calc_matrix, file = "D:/Documents/LiU Final/Autumn 21/732A94 - Advanced R/Labs/dpcheck1.csv")
