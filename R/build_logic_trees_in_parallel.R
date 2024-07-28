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
                                          no_improvement_threshold, num_cores, two_way_logic_roots, beam_width) {
  cl <- makeCluster(num_cores)
  # Ensure necessary packages are loaded on each worker
  clusterEvalQ(cl, {
    library(dplyr)
    library(magrittr)
    library(Rcpp)
    library(LogicHAL)
  })

  # Calculate the number of iterations per core
  iterations_per_core <- ceiling(max_iterations / num_cores)
  precomputed_temperatures <- compute_temperature_schedule(max_iterations, max_temperature, min_temperature, "logarithmic")

  # Split the precomputed temperatures across cores
  temperature_splits <- split(precomputed_temperatures, ceiling(seq_along(precomputed_temperatures) / iterations_per_core))


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

  # Export objects to the cluster
  clusterExport(cl, c("data", "outcome", "columns", "max_trees", "max_operators",
                      "max_temperature", "min_temperature", "iterations_per_core", "precomputed_temperatures",
                      "no_improvement_threshold", "two_way_logic_roots", "beam_width", "temperature_splits" ), envir = environment())

  results <- parLapply(cl, 1:num_cores, function(core_id) {
    run_single_iteration(data, outcome, columns, max_operators, max_trees, temperatures = temperature_splits[[core_id]] , two_way_logic_roots, beam_width, no_improvement_threshold)
  })
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

  # Select the top max_trees results
  top_results <- head(combined_results, max_trees)

  return(top_results)
}
