#' Compute Scores with Logic
#'
#' This function computes scores for given data and columns using different methods
#' based on the type of outcome (binary or continuous).
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param columns A character vector specifying the names of the columns to be used in the computation.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param previous_rule A logical vector representing the previous rule applied. Default is NULL.
#' @param previous_rule_name A character string specifying the name of the previous rule. Default is an empty string.
#'
#' @return A list containing the computed scores and the corresponding rules.
#' @examples
#' scores <- computeScores(data = df, columns = colnames(X), outcome = "outcome", previous_rule = NULL, previous_rule_name = "")
#' @export
computeScores <- function(data, columns, outcome, previous_rule, previous_rule_name) {
  is_binary <- length(unique(data[[outcome]])) == 2
  if (is_binary) {
    return(computeF1ScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name))
  } else {
    return(computeMeanDiffScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name))
  }
}

#' Count Logical Operators
#'
#' This function counts the number of logical operators (& and |) in a given rule string.
#'
#' @param rule A character string representing the rule.
#'
#' @return An integer representing the total count of logical operators in the rule.
#' @examples
#' num_operators <- count_logical_operators("(var1 == 1) & (var2 == 1) | (var3 == 1)")
#' @importFrom stringr str_count
#' @export
count_logical_operators <- function(rule) {
  sum(str_count(rule, "&")) + sum(str_count(rule, "\\|"))
}
