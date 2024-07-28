#' Run a Single Iteration of the Logic Tree Building Process
#'
#' This function runs a single iteration of the logic tree building process, which involves recursively
#' building logic trees with a U-shaped temperature schedule for simulated annealing.
#'
#' @param data Data frame. The dataset to be used for building the logic tree.
#' @param outcome Character. The name of the outcome variable.
#' @param columns Character vector. The names of the columns to be used for building the logic rules.
#' @param max_operators Integer. The maximum depth of the logic tree.
#' @param initial_temperature Numeric. The initial temperature for the simulated annealing process.
#' @param min_temperature Numeric. The minimum temperature in the middle of the iterations.
#' @param max_iterations Integer. The maximum number of iterations to run the logic tree building process.
#'
#' @return List. A list containing the best scores and best rule paths found during the iteration.
#'
#' @examples
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = sample(0:1, 100, replace = TRUE))
#' run_single_iteration(data, outcome = "y", columns = c("x1", "x2"), max_operators = 3, initial_temperature = 1, min_temperature = 0.01, max_iterations = 100)
#'
#' @export
run_single_iteration <- function(data, outcome, columns, max_operators, max_trees, temperatures, two_way_logic_roots, beam_width, no_improvement_threshold) {
  best_info_env <- new.env()
  best_info_env$best_scores <- rep(-Inf, max_trees)
  best_info_env$best_rule_paths <- vector("list", max_trees)
  best_info_env$best_complexities <- rep(Inf, max_trees)

  total_iterations <- 0
  iteration_counter <- 0
  no_improvement_counter <- 0
  previous_best_info <- list(scores = best_info_env$best_scores, paths = best_info_env$best_rule_paths)

  max_iterations <- length(temperatures)

  while (total_iterations < max_iterations && no_improvement_counter < no_improvement_threshold) {
    external_temperature <- temperatures[total_iterations + 1]
    iteration_time <- system.time({
      result <- recursive_build_logic_tree(data, outcome, columns, max_operators, current_operators = 1, parent_score = 0, previous_rule = NULL, previous_rule_name = "", best_info_env = best_info_env, external_temperature, two_way_logic_roots, beam_width, max_trees)
    })

    total_iterations <- total_iterations + 1
    iteration_counter <- iteration_counter + 1

    # Check for improvement
    if (!identical(previous_best_info$scores, best_info_env$best_scores)) {
      no_improvement_counter <- 0
      previous_best_info$scores <- best_info_env$best_scores
    } else {
      no_improvement_counter <- no_improvement_counter + 1
    }
  }

  return(list(
    best_scores = best_info_env$best_scores,
    best_rule_paths = best_info_env$best_rule_paths
  ))
}
