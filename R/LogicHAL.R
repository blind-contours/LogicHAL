#' LogicHAL: Iterative Tree Building and Lasso Fitting
#'
#' This function iteratively builds decision trees to optimize the F1 score, extracts paths as features,
#' and fits a Lasso model. The process is repeated for a specified number of trees, updating the model
#' and removing used features at each iteration.
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param columns A character string indicating which columns to construct logic interactions
#' @param max_trees An integer specifying the maximum number of trees to build. Default is 5.
#' @param max_depth An integer specifying the maximum depth of each tree. Default is 3.
#' @param epsilon A numeric value specifying the improvement threshold for early stopping. Default is 1e-4.
#'
#' @return A list containing the final Lasso model and the logic trees used in the model.
#' @examples
#' # Assuming 'df' is your data frame and 'AGO_PR' is your outcome variable
#' columns <- colnames(df)[grep("^KRFP", colnames(df))]
#' result <- LogicHAL(df, "outcome", max_trees = 5, max_depth = 3)
#' print(result$model)
#' print(result$trees)
#' @import glmnet
#' @useDynLib LogicHAL
#' @importFrom Rcpp evalCpp
#' @importFrom stringr str_extract_all
#' @export
LogicHAL <- function(data, outcome, columns, max_trees = 5, max_depth = 3, family = "gaussian", num_knots = 10) {
  # Helper function to fit Lasso
  fit_lasso <- function(features, outcome, family) {
    model <- cv.glmnet(as.matrix(features), outcome, family = family, alpha = 1)
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


  # Build next best tree
  best_info_env <- new.env()
  best_info_env$best_scores <- rep(0, max_trees)
  best_info_env$best_rule_paths <- list()
  best_info_env$best_complexities <- rep(Inf, max_trees)


  result <- build_logic_tree(data = data, outcome = outcome, columns = columns, max_depth = max_depth, best_info_env = best_info_env, max_trees = max_trees)
  trees <- result$best_rule_path
  scores <- result$best_scores

  # Extract features from paths
  path_features <- sapply(trees, function(path) as.numeric(eval(parse(text = path), envir = data)))
  path_features <- as.matrix(path_features)
  colnames(path_features) <- trees
  # Fit Lasso with all combined features
  lasso_model <- fit_lasso(path_features, data[[outcome]], family)

  return(list(model = lasso_model, trees = trees, scores = scores))
}
