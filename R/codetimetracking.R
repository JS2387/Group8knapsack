#track brute_force runtime with 60 sec delay

test_df <- knapsack_objects(1000000)

start_time <- Sys.time()
Sys.sleep(60)
knapsack_brute_force(x = test_df[1:20,], W = 2000)
end_time <- Sys.time()
print(difftime(end_time, start_time, units = "secs"))

start_time <- Sys.time()
Sys.sleep(60)
knapsack_dynamic(x = test_df[1:500,], W = 2000)
end_time <- Sys.time()
print(difftime(end_time, start_time, units = "secs"))

start_time <- Sys.time()
Sys.sleep(60)
greedy_knapsack(x = test_df, W = 3500000)
end_time <- Sys.time()
print(difftime(end_time, start_time, units = "secs"))
