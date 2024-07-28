#' LogicHAL: Iterative Tree Building and Lasso Fitting
#'
#' This function iteratively builds decision trees to optimize the F1 score, extracts paths as features,
#' and fits a Lasso model. The process is repeated for a specified number of trees, updating the model
#' and removing used features at each iteration.
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param columns A character vector indicating which columns to construct logic interactions.
#' @param max_trees An integer specifying the maximum number of trees to build. Default is 5.
#' @param max_operators An integer specifying the maximum depth (based on logic operators) of each tree. Default is 3.
#' @param family A character string specifying the family for the Lasso model. Default is "gaussian".
#' @param num_knots An integer specifying the number of knots for creating basis functions. Default is 10.
#' @param max_temperature Numeric. The maximum temperature for simulated annealing. Default is 1.
#' @param min_temperature Numeric. The minimum temperature for simulated annealing. Default is 0.001.
#' @param max_iterations Integer. The maximum number of iterations for the tree-building process. Default is 100.
#' @param no_improvement_threshold Integer. The number of iterations without improvement to stop the process. Default is 10.
#' @param num_cores Integer. The number of cores to use for parallel processing. Default is one less than the number of available cores.
#'
#' @return A list containing the final Lasso model and the logic trees used in the model.
#' @examples
#' # Assuming 'df' is your data frame and 'AGO_PR' is your outcome variable
#' columns <- colnames(df)[grep("^KRFP", colnames(df))]
#' result <- LogicHAL(df, "AGO_PR", columns, max_operators = 5, max_depth = 3)
#' print(result$model)
#' print(result$trees)
#' @import glmnet
#' @useDynLib LogicHAL
#' @importFrom Rcpp evalCpp
#' @importFrom stringr str_extract_all
#' @export
LogicHAL <- function(data, outcome, columns, max_trees = 5, max_operators = 3, family = "gaussian",
                     num_knots = 10, max_temperature = 1, min_temperature = 0.001,
                     max_iterations = 100, no_improvement_threshold = 10, beam_width = 3,
                     num_cores = detectCores() - 1, use_not_columns = TRUE) {
  # Helper function to fit Lasso
  fit_lasso <- function(features, outcome, family) {
    model <- cv.glmnet(as.matrix(features), outcome, family = family)
    return(model)
  }

  # Helper function to check if a column is binary
  is_binary_column <- function(column) {
    unique_values <- unique(column)
    return(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))
  }

  # Identify non-binary columns
  non_binary_columns <- columns[!sapply(data[columns], is_binary_column)]

  if (length(non_binary_columns) > 0) {
    # Create basis functions for non-binary columns
    basis_functions <- create_basis_functions(data, non_binary_columns, num_knots)
    basis_function_names <- colnames(basis_functions)

    # Replace non-binary columns in the original data
    data <- cbind(data, basis_functions)
    columns <- c(setdiff(columns, non_binary_columns), basis_function_names)
  }

  if (use_not_columns == TRUE) {
      not_data <- 1 - data[columns]
      # Rename columns to indicate "Not"
      colnames(not_data) <- paste0("Not_", colnames(not_data))
      # Combine original and "Not" columns
      data <- cbind(data, not_data)
      columns <- c(columns, colnames(not_data))
  }

  # Start the timer and run the function
  time_taken_pairs <- system.time({
    two_way_logic_roots <- compute_pairwise_logic_interactions_parallel(data, columns, outcome, num_cores = num_cores)
  })

  elapsed_time_pairs <- time_taken_pairs["elapsed"]

  # time_taken_triplets <- system.time({
  #   three_way_logic_roots <- compute_three_way_interactions_parallel(data, columns, outcome, num_cores = num_cores)
  # })

  result <- build_logic_trees_in_parallel(data, outcome, columns, max_operators, max_trees,
                                          max_temperature, min_temperature, max_iterations,
                                          no_improvement_threshold, num_cores, two_way_logic_roots, beam_width)
  trees <- result$rule_path
  scores <- result$score
  complexities <- result$complexity


  # Extract features from paths
  path_features <- sapply(trees, function(path) as.numeric(eval(parse(text = path), envir = data)))
  path_features <- as.matrix(path_features)
  colnames(path_features) <- trees
  # Fit Lasso with all combined features
  lasso_model <- fit_lasso(path_features, data[[outcome]], family)

  return(list(model = lasso_model, trees = trees, scores = scores))
}
