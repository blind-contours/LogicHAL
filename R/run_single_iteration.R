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
run_single_iteration <- function(data, outcome, columns, max_operators, max_trees, temperatures, two_way_logic_roots, beam_width, no_improvement_threshold, previous_rule_name, best_info_env) {

  temp_index_env <- new.env()
  temp_index_env$current_index <- 1  # Initialize the index

  no_improvement_counter <- 0
  previous_best_info <- list(scores = best_info_env$best_scores, paths = best_info_env$best_rule_paths)

  max_iterations <- length(temperatures)

  if (is.null(previous_rule_name)) {
    parent_rule_names <- list()
    parent_rules <- list()
    parent_scores <- list()
  } else{
    parent_rule_names <- list()
    parent_rules <- list()
    parent_scores <- list()
    parent_rule_names[[1]] <- previous_rule_name

    previous_rule <- data %>%
      mutate(rule = eval(parse(text = previous_rule_name))) %>%
      pull(rule)

    parent_scores[[1]] <- calculate_f1_score(previous_rule, data[[outcome]])

    parent_rules[[1]] <- previous_rule
  }

  # Flag to check if first iteration with previous rule
  first_iteration_with_previous <- TRUE

  while (temp_index_env$current_index < max_iterations && no_improvement_counter < no_improvement_threshold) {
    iteration_time <- system.time({
      result <- recursive_build_logic_tree(data, outcome, columns, max_operators,
                                           current_operators = 1,
                                           parent_scores = parent_scores,
                                           parent_rules = parent_rules,
                                           parent_rule_names = parent_rule_names,
                                           best_info_env = best_info_env,
                                           temperatures = temperatures,
                                           temp_index_env = temp_index_env,
                                           two_way_logic_roots,
                                           beam_width, max_trees)
    })

    # Check for improvement
    if (!identical(previous_best_info$scores, best_info_env$best_scores)) {
      no_improvement_counter <- 0
      previous_best_info$scores <- best_info_env$best_scores

      # If improvement is made, continue using the previous rule
      first_iteration_with_previous <- FALSE
    } else {
      no_improvement_counter <- no_improvement_counter + 1

      # Reset to logic roots if no improvement and it's not the first iteration with the previous rule
      if (first_iteration_with_previous) {
        previous_rule_name <- ""
        previous_rule <- NULL
        first_iteration_with_previous <- FALSE
      }
    }
  }

  return(best_info_env)
}
