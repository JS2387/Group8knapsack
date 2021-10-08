#' @title greedy_knapsack function
#' @description evaluates the solution for knapsack problem by choosing elements with highest value per unit weight and using complete knapsack capacity
#' @returns list containing sum of values of chosen elements and the reference of the elements chosen in the final solution
#' @param x as a {\code}data.frame containing the weights and values of knapsack objects with column names 'w' & 'v' respectively
#' @param W as a {\code}numeric value defining the constraint of the knapsack capacity
#' @export
greedy_knapsack <- function(x, W) {

  kp_objects <- x                                                        #assign data to temp variable
  kp_objects$normvalue <- kp_objects$v/kp_objects$w                      #create profit/item field
  kp_objects$rank[order(-kp_objects$normvalue)] <- 1:nrow(kp_objects)    #create column containing rank of all objects in descending order of profit
  n = length(kp_objects$w)                                               #count of objects
  kp_objects$order <- c(1:n)                                             #preserve original object position before sorting

  kp_objects <- kp_objects[order(kp_objects$rank),]                      #sort data by rank

  #store all fields separately for ease of referencing
  w <- kp_objects$w
  v <- kp_objects$v
  normv <- kp_objects$normvalue
  rank <- kp_objects$rank
  order <- kp_objects$order
  cap_left = W

  #define result variables
  chosen_elements <- c()
  final_value <- 0
  result_list <- list(value = 0, elements = c())

  #loop over algorithm
  for (i in 1:n) {
    if (w[i] <= cap_left) {                                              #if item is within capacity left
      chosen_elements <- c(chosen_elements, order[i])                    #store original object position
      final_value = final_value + v[i]                                   #all final value with value of chosen object
      cap_left = cap_left - w[i]                                         #reduce capacity left by weight of chosen object
    }
    if (w[i] > cap_left) {                                               #if item weight is greater than the capacity left
      chosen_elements <- c(chosen_elements, order[i])                    #store element position
      final_value = final_value + cap_left * normv[i]                    #add fractional value of final object to result
      break
    }
  }

  #store the result
  result_list$value = final_value
  result_list$elements <- chosen_elements

  return(result_list)
}
