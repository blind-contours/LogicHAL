# Function to compute scores with logic
computeScores <- function(data, columns, outcome, previous_rule, previous_rule_name) {
  is_binary <- length(unique(data[[outcome]])) == 2
  if (is_binary) {
    return(computeF1ScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name))
  } else {
    return(computeMeanDiffScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name))
  }
}


# Function to count logical operators
count_logical_operators <- function(rule) {
  sum(str_count(rule, "&")) + sum(str_count(rule, "\\|"))
}
