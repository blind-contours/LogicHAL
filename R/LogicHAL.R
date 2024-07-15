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
LogicHAL <- function(data, outcome, columns, max_trees = 5, max_depth = 3) {
  # Helper function to fit Lasso
  fit_lasso <- function(features, outcome) {
    model <- cv.glmnet(as.matrix(features), as.factor(outcome), family = "binomial", alpha = 1)
    return(model)
  }

  current_data <- data
  all_features <- NULL
  trees <- list()
  scores <- list()
  used_columns <- c()
  remaining_columns <- columns

  for (i in 1:max_trees) {
    # Build next best tree
    best_info_env <- new.env()
    best_info_env$best_f1 <- 0
    best_info_env$best_rule_path <- list()

    result <- build_logic_tree(data = current_data, outcome = outcome, columns = remaining_columns, max_depth = max_depth, best_info_env = best_info_env)
    trees[[i]] <- result$best_rule_path
    scores[[i]] <- result$best_f1

    # Extract features from paths
    path_features <- sapply(result$best_rule_path, function(path) eval(parse(text = path), envir = data))

    # Combine features
    if (is.null(all_features)) {
      all_features <- path_features
    } else {
      all_features <- cbind(all_features, path_features)
    }

    # Correctly extract used columns from the rule paths
    used_columns <- unique(c(used_columns, unlist(lapply(result$best_rule_path, function(path) {
      # Extract variable names using exact match regex pattern
      str_extract_all(path, paste0("\\b", colnames(data), "\\b"))
    }))))

    # Update remaining columns
    remaining_columns <- setdiff(columns, used_columns)
    current_data <- data[, remaining_columns, drop = FALSE]
    current_data[[outcome]] <- data[[outcome]]
  }

  # Fit Lasso with all combined features
  lasso_model <- fit_lasso(all_features, data[[outcome]])

  return(list(model = lasso_model, trees = trees, scores = scores))
}
