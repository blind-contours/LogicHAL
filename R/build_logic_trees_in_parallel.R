#' Build Logic Trees in Parallel
#'
#' This function executes the logic tree building process in parallel across
#' multiple cores, combining results from each core to find the best
#' logic trees.
#'
#' @param data Data frame. The dataset to be used for building the logic trees.
#' @param outcome Character. The name of the outcome variable.
#' @param columns Character vector. The names of the columns to be used for building the logic rules.
#' @param max_operators Integer. The maximum depth of the logic trees based on logical operators. Default is 3.
#' @param max_trees Integer. The maximum number of top trees to retain. Default is 5.
#' @param initial_temperature Numeric. The initial temperature for the simulated annealing process. Default is 1.
#' @param min_temperature Numeric. The minimum temperature for the simulated annealing process. Default is 0.001.
#' @param max_iterations Integer. The maximum number of iterations to run the logic tree building process. Default is 1000.
#' @param no_improvement_threshold Integer. The number of iterations without improvement before stopping. Default is 100.
#' @param num_cores Integer. The number of cores to use for parallel processing. Default is `detectCores() - 1`.
#'
#' @return Data frame. A data frame containing the best scores, rule paths, and complexities of the top logic trees.
#'
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = sample(0:1, 100, replace = TRUE))
#' build_logic_trees_in_parallel(data, outcome = "y", columns = c("x1", "x2"), max_operators = 3, max_trees = 5)
#' @import parallel
#' @export
build_logic_trees_in_parallel <- function(data, outcome, columns, max_operators, max_trees,
                                           max_temperature, min_temperature, max_iterations,
                                          no_improvement_threshold, num_cores, two_way_logic_roots,
                                          beam_width, n_exchange, lasso_stability_threshold, family) {

  # Initialize a list of environments for each core
  best_info_env_list <- lapply(seq_len(num_cores), function(i) { # include an argument i
    env <- new.env()
    env$best_scores <- rep(-Inf, max_trees)
    env$best_rule_paths <- vector("list", max_trees)
    env$best_complexities <- rep(Inf, max_trees)
    env$temperature <- rep(Inf, max_trees)
    return(env)
  })


  cl <- makeCluster(num_cores)
  # Ensure necessary packages are loaded on each worker
  clusterEvalQ(cl, {
    library(dplyr)
    library(magrittr)
    library(Rcpp)
    library(LogicHAL)
  })


  # Compute the temperature schedule and split it among the cores
  precomputed_temperatures <- compute_temperature_schedule(max_iterations, max_temperature, min_temperature, "exponential")
  split_temps <- split(precomputed_temperatures, rep(1:num_cores, each = length(precomputed_temperatures) / num_cores))

  # Function to split temperatures for each exchange within each core
  split_temperatures_for_exchange <- function(temperatures, n_exchange) {
    n <- length(temperatures)
    num_segments <- ceiling(n / n_exchange)
    return(split(temperatures, rep(1:n_exchange, each = num_segments, length.out = n)))
  }

  # Apply the nested splitting for each core
  temperature_schedule_list <- lapply(split_temps, split_temperatures_for_exchange, n_exchange)
  # Evaluate function parameters
  data <- data
  outcome <- outcome
  columns <- columns
  max_operators <- max_operators
  max_trees <- max_trees
  max_temperature <- max_temperature
  min_temperature <- min_temperature
  max_iterations <- max_iterations
  no_improvement_threshold <- no_improvement_threshold
  beam_width <- beam_width
  exchange_rules <- vector("list", num_cores)

  # Export objects to the cluster
  clusterExport(cl, c("data", "outcome", "columns", "max_trees", "max_operators", "max_iterations",
                      "max_temperature", "min_temperature", "precomputed_temperatures",
                      "no_improvement_threshold", "two_way_logic_roots", "beam_width",
                      "temperature_schedule_list", "n_exchange", "exchange_rules", "best_info_env_list"), envir = environment())

  results <- list()

  stable_nonzero_coefs <- NULL
  stability_counter <- 0


  for (exchange_iteration in 1:n_exchange) {
    iteration_results <- parLapply(cl, 1:num_cores, function(core_id) {
      run_single_iteration(data,
                           outcome,
                           columns,
                           max_operators,
                           max_trees,
                           temperatures = temperature_schedule_list[[core_id]][[exchange_iteration]],
                           two_way_logic_roots,
                           beam_width,
                           no_improvement_threshold,
                           previous_rule_name = exchange_rules[[core_id]],
                           best_info_env = best_info_env_list[[core_id]]
                           )
    })

    # Initialize lists to store data
    temperature_window <- c()
    best_scores <- c()
    best_rule_paths <- c()
    temperatures <- c()

    # Loop through the list and extract the necessary information
    for (i in seq_along(iteration_results)) {
      temperature_window <- c(temperature_window, rep(i, length(iteration_results[[i]]$best_scores)))
      best_scores <- c(best_scores, iteration_results[[i]]$best_scores)
      best_rule_paths <- c(best_rule_paths, iteration_results[[i]]$best_rule_paths)
      temperatures <- c(temperatures, iteration_results[[i]]$temperature)
    }

    # Create the data frame
    results_df <- data.frame(
      temperature_window = temperature_window,
      best_scores = best_scores,
      best_rule_paths = best_rule_paths,
      temperatures = temperatures
    )

    results <- append(results, iteration_results)

    # Convert logic paths to binary feature matrix
    path_features <- sapply(results_df$best_rule_paths, function(path) as.numeric(eval(parse(text = path), envir = data)))
    path_features <- as.matrix(path_features)
    colnames(path_features) <- results_df$best_rule_paths

    # Fit Lasso with all combined features
    lasso_model <- cv.glmnet(path_features, data[[outcome]], family = family)  # Use binomial for binary outcome

    # Get the coefficients from the Lasso model
    lasso_coefs <- coef(lasso_model, s = "lambda.1se")

    # Convert the sparse matrix to a regular matrix to work with it
    coefs_matrix <- as.matrix(lasso_coefs)

    # Extract the names of non-zero coefficients
    nonzero_coefs <- rownames(coefs_matrix)[coefs_matrix != 0]

    # If you also want to get the values of these non-zero coefficients
    nonzero_values <- coefs_matrix[coefs_matrix != 0]
    # Check if non-zero coefficients are stable
    if (!is.null(stable_nonzero_coefs) && identical(sort(nonzero_coefs), sort(stable_nonzero_coefs))) {
      stability_counter <- stability_counter + 1
      if (stability_counter >= lasso_stability_threshold) {
        message("Early stopping: Non-zero coefficients in Lasso model are stable.")
        break
      }
    } else {
      stable_nonzero_coefs <- nonzero_coefs
      stability_counter <- 0
    }

    # Iterate through the temperature windows and decide swaps
    for (i in 1:(nrow(results_df) - 1)) {
      # Get current and next windows
      current_window <- results_df$temperature_window[i]
      next_window <- results_df$temperature_window[i + 1]

      if (current_window == next_window - 1) {
        # Calculate swap probability
        swap_probability <- calculate_swap_probability(
          results_df$best_scores[i],
          results_df$best_scores[i + 1],
          results_df$temperatures[i],
          results_df$temperatures[i + 1]
        )

        # Decide swap
        if (runif(1) < swap_probability) {
          # Store swap in exchange_rules list
          exchange_rules[[next_window]] <- results_df$best_rule_paths[i]
          exchange_rules[[current_window]] <- results_df$best_rule_paths[i + 1]
        } else {
          # If no swap, retain original assignments
          exchange_rules[[current_window]] <- ""
          exchange_rules[[next_window]] <- ""
        }
      }
    }

    best_info_env_list <- iteration_results
  }
  stopCluster(cl)

  # Combine results from all parallel processes
  combined_results <- data.frame(
    score = numeric(),
    rule_path = I(list()),
    complexity = integer()
  )

  rule_set <- list()

  for (result in results) {
    for (i in 1:max_trees) {
      new_rule_path <- result$best_rule_paths[[i]]
      new_score <- result$best_scores[i]
      new_complexity <- count_logical_operators(new_rule_path)

      # Check if the rule is already in the combined results
      rule_already_exists <- any(sapply(rule_set, function(rule) identical(rule, new_rule_path)))

      if (!rule_already_exists) {
        new_row <- data.frame(
          score = new_score,
          rule_path = I(list(new_rule_path)),
          complexity = new_complexity
        )
        combined_results <- rbind(combined_results, new_row)
        rule_set <- c(rule_set, list(new_rule_path))
      }
    }
  }

  # Sort the combined results by score (and complexity in case of ties)
  combined_results <- combined_results[order(-combined_results$score, combined_results$complexity), ]


  return(combined_results)
}
