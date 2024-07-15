#' Build Logic Tree for Optimizing F1 Score
#'
#' This function builds a decision tree by recursively finding the best splits to optimize the F1 score.
#' It searches for logical interactions (AND and OR statements) among the features,
#' and calculates a weighted F1 score along the paths of the tree.
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param columns A character vector of feature names to be considered for splitting.
#' @param max_depth An integer specifying the maximum depth of the tree. Default is 3.
#' @param current_depth An integer specifying the current depth of the tree. Default is 1.
#' @param path_f1 A numeric value representing the F1 score of the current path. Default is 0.
#' @param path_weight A numeric value representing the weight of the current path (number of observations). Default is 0.
#'
#' @return A list representing the tree structure with splits, F1 scores, and path F1 scores.
#' @import dplyr
#' @export
#' @examples
#' # Assuming 'df' is your data frame and 'AGO_PR' is your outcome variable
#' columns <- colnames(df)[grep("^KRFP", colnames(df))]
#' tree <- build_logic_tree(df, "AGO_PR", columns, max_depth = 3)
#' print(tree)
build_logic_tree <- function(data, outcome, columns, max_depth = 3, current_depth = 1,
                             parent_f1 = 0, previous_rule = NULL, previous_rule_name = "",
                             best_info_env) {
  # Base case: max depth reached or no data
  if (current_depth > max_depth || nrow(data) == 0) {
    return(list(best_f1 = best_info_env$best_f1, best_rule_path = best_info_env$best_rule_path, subtrees = NULL))
  }

  is_binary <- length(unique(data[[outcome]])) == 2

  if (is_binary) {
    precomputed_scores <- computeF1ScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
  } else {
    precomputed_scores <- computeMeanDifferenceScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
  }

  # Extract results
  F1_indiv_ruleAnd <- precomputed_scores$F1_indiv_ruleAnd
  F1_indiv_ruleOr <- precomputed_scores$F1_indiv_ruleOr

  F1_And <- precomputed_scores$F1And
  F1_Or <- precomputed_scores$F1Or
  F1AndAnd <- precomputed_scores$F1AndAnd
  F1AndOr <- precomputed_scores$F1AndOr
  F1OrAnd <- precomputed_scores$F1OrAnd
  F1OrOr <- precomputed_scores$F1OrOr

  Rule_indiv_And <- precomputed_scores$Rule_indiv_And
  Rule_indiv_Or <- precomputed_scores$Rule_indiv_Or
  RuleAnd <- precomputed_scores$RuleAnd
  RuleOr <- precomputed_scores$RuleOr
  RuleAndAnd <- precomputed_scores$RuleAndAnd
  RuleAndOr <- precomputed_scores$RuleAndOr
  RuleOrAnd <- precomputed_scores$RuleOrAnd
  RuleOrOr <- precomputed_scores$RuleOrOr

  # Initialize list to store subtrees
  subtrees <- list()

  # Function to create a subtree
  create_subtree <- function(F1_score, rule, type) {
    if (any(F1_score != 0, na.rm = TRUE)) {
      max_f1_index <- which.max(F1_score)
      best_root_f1 <- F1_score[max_f1_index]
      root_rule_desc <- rule[max_f1_index]

      if (best_root_f1 >= parent_f1) {
        # Evaluate the root rule using dplyr
        root_rule <- data %>%
          mutate(rule = eval(parse(text = root_rule_desc))) %>%
          pull(rule)

        # Update the best F1 and best rule path if this is the best so far
        if (best_root_f1 > best_info_env$best_f1) {
          best_info_env$best_f1 <- best_root_f1
          best_info_env$best_rule_path <- root_rule_desc
        } else if (best_root_f1 == best_info_env$best_f1) {
          # Tie-breaking logic: prioritize rules with fewer logical operators
          current_complexity <- count_logical_operators(best_info_env$best_rule_path)
          new_complexity <- count_logical_operators(root_rule_desc)
          if (new_complexity < current_complexity) {
            best_info_env$best_rule_path <- root_rule_desc
          } else if (new_complexity == current_complexity) {
            best_info_env$best_rule_path <- c(best_info_env$best_rule_path, root_rule_desc)
          }
        }

        # Recursive call for the subtree
        subtree_result <- build_logic_tree(data = data, outcome = outcome, columns = columns,
                                           max_depth = max_depth, current_depth = current_depth + 1,
                                           parent_f1 = best_root_f1, previous_rule = root_rule,
                                           previous_rule_name = root_rule_desc, best_info_env = best_info_env)

        return(list(
          split = root_rule_desc,
          f1 = round(best_root_f1, 3),
          subtree = subtree_result$tree
        ))
      }
    }
    return(NULL)
  }

  # Create subtrees for each type
  subtrees$indiv_ruleAnd <- create_subtree(F1_score = F1_indiv_ruleAnd, rule = Rule_indiv_And, type = "indiv_ruleAnd")
  subtrees$indiv_ruleOr <- create_subtree(F1_indiv_ruleOr, Rule_indiv_Or, "indiv_ruleOr")

  subtrees$And <- create_subtree(F1_score = F1_And, rule = RuleAnd, type = "And")
  subtrees$Or <- create_subtree(F1_score = F1_Or, rule = RuleOr, type = "Or")

  subtrees$AndAnd <- create_subtree(F1AndAnd, RuleAndAnd, "AndAnd")
  subtrees$AndOr <- create_subtree(F1AndOr, RuleAndOr, "AndOr")
  subtrees$OrAnd <- create_subtree(F1OrAnd, RuleOrAnd, "OrAnd")
  subtrees$OrOr <- create_subtree(F1OrOr, RuleOrOr, "OrOr")

  # Create the tree structure
  tree <- list(
    subtrees = subtrees,
    best_f1 = best_info_env$best_f1,
    best_rule_path = best_info_env$best_rule_path
  )

  return(list(
    tree = tree,
    best_f1 = best_info_env$best_f1,
    best_rule_path = best_info_env$best_rule_path
  ))
}
