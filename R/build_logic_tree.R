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
                             parent_score = 0, previous_rule = NULL, previous_rule_name = "",
                             best_info_env, max_trees = 5) {
  # Base case: max depth reached or no data
  if (current_depth > max_depth || nrow(data) == 0) {
    return(list(best_scores = best_info_env$best_scores, best_rule_paths = best_info_env$best_rule_paths, subtrees = NULL))
  }

  is_binary <- length(unique(data[[outcome]])) == 2

  if (is_binary) {
    precomputed_scores <- computeF1ScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
    # Extract results
    score_indiv_ruleAnd <- precomputed_scores$F1_indiv_ruleAnd
    score_indiv_ruleOr <- precomputed_scores$F1_indiv_ruleOr

    score_And <- precomputed_scores$F1And
    score_Or <- precomputed_scores$F1Or

    score_AndAnd <- precomputed_scores$F1AndAnd
    score_AndOr <- precomputed_scores$F1AndOr
    score_OrAnd <- precomputed_scores$F1OrAnd
    score_OrOr <- precomputed_scores$F1OrOr

    Rule_indiv_And <- precomputed_scores$Rule_indiv_And
    Rule_indiv_Or <- precomputed_scores$Rule_indiv_Or
    RuleAnd <- precomputed_scores$RuleAnd
    RuleOr <- precomputed_scores$RuleOr
    RuleAndAnd <- precomputed_scores$RuleAndAnd
    RuleAndOr <- precomputed_scores$RuleAndOr
    RuleOrAnd <- precomputed_scores$RuleOrAnd
    RuleOrOr <- precomputed_scores$RuleOrOr
  } else {
    precomputed_scores <- computeMeanDifferenceScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
    score_indiv_ruleAnd <- precomputed_scores$MeanDiff_indiv_ruleAnd
    score_indiv_ruleOr <- precomputed_scores$MeanDiff_indiv_ruleOr

    score_And <- precomputed_scores$MeanDiffAnd
    score_Or <- precomputed_scores$MeanDiffOr

    score_AndAnd <- precomputed_scores$MeanDiffAndAnd
    score_AndOr <- precomputed_scores$MeanDiffAndOr
    score_OrAnd <- precomputed_scores$MeanDiffOrAnd
    score_OrOr <- precomputed_scores$MeanDiffOrOr

    Rule_indiv_And <- precomputed_scores$Rule_indiv_And
    Rule_indiv_Or <- precomputed_scores$Rule_indiv_Or
    RuleAnd <- precomputed_scores$RuleAnd
    RuleOr <- precomputed_scores$RuleOr
    RuleAndAnd <- precomputed_scores$RuleAndAnd
    RuleAndOr <- precomputed_scores$RuleAndOr
    RuleOrAnd <- precomputed_scores$RuleOrAnd
    RuleOrOr <- precomputed_scores$RuleOrOr
  }

  # Initialize list to store subtrees
  subtrees <- list()

  # Function to update the best scores and rule paths in best_info_env
  update_best_info <- function(score, rule_desc) {
    current_best_scores <- best_info_env$best_scores
    current_best_rules <- best_info_env$best_rule_paths
    current_best_complexities <- best_info_env$best_complexities
    new_complexity <- count_logical_operators(rule_desc)

    # Check if the new score is better than or equal to the minimum score in the current best scores
    if (score >= min(current_best_scores)) {
      if (score %in% current_best_scores) {
        # If the score ties with any current best score, consider complexity
        tied_indices <- which(current_best_scores == score)
        for (index in tied_indices) {
          current_rule <- current_best_rules[[index]]
          current_complexity <- current_best_complexities[index]
          if (new_complexity < current_complexity) {
            current_best_rules[[index]] <- rule_desc
            current_best_complexities[index] <- new_complexity
            break
          } else if (new_complexity == current_complexity) {
            current_best_rules <- current_best_rules
            current_best_complexities <- current_best_complexities
            break
          }
        }
      } else {
        # If the new score is better than the minimum score, replace it
        min_index <- which.min(current_best_scores)
        current_best_scores[min_index] <- score
        current_best_rules[[min_index]] <- rule_desc
        current_best_complexities[min_index] <- new_complexity
      }

      # Sort based on scores and then by complexity
      sorted_indices <- order(current_best_scores, -current_best_complexities, decreasing = TRUE)
      best_info_env$best_scores <- current_best_scores[sorted_indices]
      best_info_env$best_rule_paths <- current_best_rules[sorted_indices]
      best_info_env$best_complexities <- current_best_complexities[sorted_indices]
    }
  }


  # Function to create a subtree
  create_subtree <- function(score, rule, type) {
    if (any(score != 0, na.rm = TRUE)) {
      max_score_index <- which.max(score)
      best_root_score <- score[max_score_index]
      root_rule_desc <- rule[max_score_index]

      if (best_root_score >= parent_score) {
        # Evaluate the root rule using dplyr
        root_rule <- data %>%
          mutate(rule = eval(parse(text = root_rule_desc))) %>%
          pull(rule)

        # Update the best scores and best rule paths if this is the best so far
        update_best_info(score = best_root_score, rule_desc = root_rule_desc)

        # Recursive call for the subtree
        subtree_result <- build_logic_tree(data = data, outcome = outcome, columns = columns,
                                           max_depth = max_depth, current_depth = current_depth + 1,
                                           parent_score = best_root_score, previous_rule = root_rule,
                                           previous_rule_name = root_rule_desc, best_info_env = best_info_env, max_trees = max_trees)

        return(list(
          split = root_rule_desc,
          score = round(best_root_score, 3),
          subtree = subtree_result$tree
        ))
      }
    }
    return(NULL)
  }

  # Create subtrees for each type
  subtrees$indiv_ruleAnd <- create_subtree(score = score_indiv_ruleAnd, rule = Rule_indiv_And, type = "indiv_ruleAnd")
  subtrees$indiv_ruleOr <- create_subtree(score = score_indiv_ruleOr, rule = Rule_indiv_Or, type = "indiv_ruleOr")

  subtrees$And <- create_subtree(score = score_And, rule = RuleAnd, type = "And")
  subtrees$Or <- create_subtree(score = score_Or, rule = RuleOr, type = "Or")

  subtrees$AndAnd <- create_subtree(score = score_AndAnd, rule = RuleAndAnd, type = "AndAnd")
  subtrees$AndOr <- create_subtree(score = score_AndOr, rule = RuleAndOr, type = "AndOr")
  subtrees$OrAnd <- create_subtree(score = score_OrAnd, rule = RuleOrAnd, type = "OrAnd")
  subtrees$OrOr <- create_subtree(score = score_OrOr, rule = RuleOrOr, type = "OrOr")

  # Create the tree structure
  tree <- list(
    subtrees = subtrees,
    best_scores = best_info_env$best_scores,
    best_rule_paths = best_info_env$best_rule_paths
  )

  return(list(
    tree = tree,
    best_scores = best_info_env$best_scores,
    best_rule_paths = best_info_env$best_rule_paths
  ))
}
