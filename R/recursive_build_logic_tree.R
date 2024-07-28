#' Recursively Build Logic Trees (Greedy Best-First Search)
#'
#' This function recursively builds logic trees by calculating scores for logical interactions among features and expanding subtrees based on those scores. The function employs a greedy best-first search strategy.
#'
#' @param data A data frame containing the dataset.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param columns A character vector of feature names to be considered for logical interactions.
#' @param max_operators An integer specifying the maximum number of logic operators for the rules.
#' @param current_operators An integer specifying the current number of logic operators in the rule (default is 0).
#' @param parent_score A numeric value representing the score of the parent rule (default is 0).
#' @param previous_rule A logical vector indicating whether each observation satisfies the previous rule (default is NULL).
#' @param previous_rule_name A character string describing the previous rule (default is "").
#' @param best_info_env An environment containing the best scores, rule paths, and complexities.
#' @param external_temperature A numeric value for the external temperature, affecting the recursion depth.
#' @param two_way_logic_roots A list containing precomputed scores and rules for two-way logic interactions.
#' @param beam_width An integer specifying the number of indices to sample (beam width).
#' @param max_trees An integer specifying the maximum number of top trees to consider (default is 10).
#'
#' @return A list containing the tree structure, best scores, and best rule paths.
#'
#' @examples
#' # Assuming `data`, `columns`, `outcome`, `best_info_env`, and `two_way_logic_roots` are predefined
#' result <- recursive_build_logic_tree(data, outcome, columns, max_operators = 3, best_info_env = best_info_env, external_temperature = 1.0, two_way_logic_roots = two_way_logic_roots)
#'
#' @export
recursive_build_logic_tree <- function(data, outcome, columns, max_operators, current_operators = 0,
                                       parent_score = 0, previous_rule = NULL, previous_rule_name = "",
                                       best_info_env, external_temperature, two_way_logic_roots, beam_width, max_trees) {
  cat("Current Operators:", current_operators, "Max Operators:", max_operators, "\n") # Debugging statement

  # Base case: max operators reached or no data
  if (current_operators >= max_operators || nrow(data) == 0) {
    return(list(best_scores = best_info_env$best_scores, best_rule_paths = best_info_env$best_rule_paths, subtrees = NULL))
  }

  # Calculate internal temperature
  internal_temperature <- external_temperature * (max_operators - current_operators + 1) / max_operators

  # Check if the outcome is binary
  is_binary <- length(unique(data[[outcome]])) == 2

  if (is.null(previous_rule)) {
    # Use precomputed two-way logic roots if no previous rule
    score_And <- two_way_logic_roots$F1And
    score_Or <- two_way_logic_roots$F1Or
    RuleAnd <- two_way_logic_roots$RuleAnd
    RuleOr <- two_way_logic_roots$RuleOr

    # Combine scores and rules into a data frame for easier manipulation
    scores_to_compare <- c(score_And, score_Or)
    rules_to_compare <- c(RuleAnd, RuleOr)

    comparison_df <- data.frame(scores = scores_to_compare, rules = rules_to_compare)
    comparison_df <- comparison_df[order(-comparison_df$scores), ]

    # Update best_info_env with the top max_trees scores and rules
    for (i in 1:min(nrow(comparison_df), max_trees)) {
      update_best_info(score = comparison_df$scores[i], rule_desc = comparison_df$rules[i], best_info_env = best_info_env)
    }
  } else {
    # Compute new scores and rules based on the previous rule
    if (is_binary) {
      precomputed_scores <- computeF1ScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
    } else {
      precomputed_scores <- computeMeanDifferenceScoresWithLogic(data, columns, data[[outcome]], previous_rule, previous_rule_name)
    }

    score_indiv_ruleAnd <- precomputed_scores$F1_indiv_ruleAnd
    score_indiv_ruleOr <- precomputed_scores$F1_indiv_ruleOr

    Rule_indiv_And <- precomputed_scores$Rule_indiv_And
    Rule_indiv_Or <- precomputed_scores$Rule_indiv_Or

    score_AndAnd <- precomputed_scores$F1AndAnd
    score_AndOr <- precomputed_scores$F1AndOr
    score_OrAnd <- precomputed_scores$F1OrAnd
    score_OrOr <- precomputed_scores$F1OrOr

    Rule_AndAnd <- precomputed_scores$RuleAndAnd
    Rule_AndOr <- precomputed_scores$RuleAndOr
    Rule_OrAnd <- precomputed_scores$RuleOrAnd
    Rule_OrOr <- precomputed_scores$RuleOrOr

    # Combine scores and rules into a data frame for easier manipulation
    scores_to_compare <- c(score_AndAnd, score_AndOr, score_OrAnd, score_OrOr)
    rules_to_compare <- c(Rule_AndAnd, Rule_AndOr, Rule_OrAnd, Rule_OrOr)

    comparison_df <- data.frame(scores = scores_to_compare, rules = rules_to_compare)
    comparison_df <- comparison_df[order(-comparison_df$scores), ]

    # Update best_info_env with the top max_trees scores and rules
    for (i in 1:min(nrow(comparison_df), max_trees)) {
      update_best_info(score = comparison_df$scores[i], rule_desc = comparison_df$rules[i], best_info_env = best_info_env)
    }
  }

  # Initialize list to store subtrees
  subtrees <- list()

  # Create subtrees for each type
  if (is.null(previous_rule)) {
    subtrees$And <- create_subtree(scores = score_And, rules = RuleAnd, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
    subtrees$Or <- create_subtree(scores = score_Or, rules = RuleOr, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
  } else {
    subtrees$indiv_ruleAnd <- create_subtree(scores = score_indiv_ruleAnd, rules = Rule_indiv_And, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
    subtrees$indiv_ruleOr <- create_subtree(scores = score_indiv_ruleOr, rules = Rule_indiv_Or, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$AndAnd <- create_subtree(scores = score_AndAnd, rules = Rule_AndAnd, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
    subtrees$AndOr <- create_subtree(scores = score_AndOr, rules = Rule_AndOr, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
    subtrees$OrAnd <- create_subtree(scores = score_OrAnd, rules = Rule_OrAnd, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
    subtrees$OrOr <- create_subtree(scores = score_OrOr, rules = Rule_OrOr, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
  }

  return(list(
    tree = list(subtrees = subtrees, best_scores = best_info_env$best_scores, best_rule_paths = best_info_env$best_rule_paths),
    best_scores = best_info_env$best_scores,
    best_rule_paths = best_info_env$best_rule_paths
  ))
}
