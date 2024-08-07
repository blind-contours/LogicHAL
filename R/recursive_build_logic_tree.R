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
                                       parent_scores = list(), parent_rules = list(), parent_rule_names = list(),
                                       best_info_env, temperatures, temp_index_env, two_way_logic_roots, beam_width, max_trees) {
  cat("Current Operators:", current_operators, "Max Operators:", max_operators, "\n") # Debugging statement

  # Base case: max operators reached or no data
  if (current_operators >= max_operators || temp_index_env$current_index >= length(temperatures)) {
    return(list(best_scores = best_info_env$best_scores, best_rule_paths = best_info_env$best_rule_paths, subtrees = NULL))
  }

  # Calculate internal temperature
  internal_temperature <- temperatures[temp_index_env$current_index]
  temp_index_env$current_index <- temp_index_env$current_index + 1  # Move to the next temperature

  # Check if the outcome is binary
  is_binary <- length(unique(data[[outcome]])) == 2

  if (length(parent_rules) == 0) {  # No previous rule, use precomputed two-way logic roots
    score_And <- c(two_way_logic_roots$F1And, unlist(parent_scores))
    score_Or <- c(two_way_logic_roots$F1Or, unlist(parent_scores))
    RuleAnd <- c(two_way_logic_roots$RuleAnd, unlist(parent_rule_names))
    RuleOr <- c(two_way_logic_roots$RuleOr,  unlist(parent_rule_names))

    # Combine scores and rules into a data frame for easier manipulation
    scores_to_compare <- as.numeric(c(score_And, score_Or))
    rules_to_compare <- c(RuleAnd, RuleOr)

    comparison_df <- data.frame(scores = scores_to_compare, rules = rules_to_compare)
    comparison_df <- comparison_df[order(-comparison_df$scores), ]

    best_info_env$best_scores <- comparison_df$scores[1:max_trees]
    best_info_env$best_rule_paths <- comparison_df$rules[1:max_trees]
    best_info_env$best_complexities <- rep(1, max_trees)
    best_info_env$temperature <- rep(internal_temperature, max_trees)

    best_trees_evaluated_list <- evaluate_all_rules(rules = best_info_env$best_rule_paths, data = data)
    score_merged_trees <- compute_merge_trees(rules = best_info_env$best_rule_paths, data = best_trees_evaluated_list, true_labels = data[[outcome]])

  } else {
    # Compute new scores and rules based on the previous rule
    if (is_binary) {
      precomputed_scores <- computeF1ScoresWithLogic(data, columns, data[[outcome]], parent_rules[[length(parent_rules)]], parent_rule_names[[length(parent_rule_names)]])
    } else {
      precomputed_scores <- computeMeanDifferenceScoresWithLogic(data, columns, data[[outcome]], parent_rules[[length(parent_rules)]], parent_rule_names[[length(parent_rule_names)]])
    }

    score_indiv_ruleAnd <- c(precomputed_scores$F1_indiv_ruleAnd, unlist(parent_scores))
    score_indiv_ruleOr <- c(precomputed_scores$F1_indiv_ruleOr, unlist(parent_scores))

    Rule_indiv_And <- c(precomputed_scores$Rule_indiv_And, unlist(parent_rule_names))
    Rule_indiv_Or <- c(precomputed_scores$Rule_indiv_Or, unlist(parent_rule_names))

    score_AndAnd <- c(precomputed_scores$F1AndAnd, unlist(parent_scores))
    score_AndOr <- c(precomputed_scores$F1AndOr, unlist(parent_scores))
    score_OrAnd <- c(precomputed_scores$F1OrAnd, unlist(parent_scores))
    score_OrOr <- c(precomputed_scores$F1OrOr, unlist(parent_scores))

    Rule_AndAnd <- c(precomputed_scores$RuleAndAnd, unlist(parent_rule_names))
    Rule_AndOr <- c(precomputed_scores$RuleAndOr, unlist(parent_rule_names))
    Rule_OrAnd <- c(precomputed_scores$RuleOrAnd, unlist(parent_rule_names))
    Rule_OrOr <- c(precomputed_scores$RuleOrOr, unlist(parent_rule_names))

    # Combine scores and rules into a data frame for easier manipulation
    scores_to_compare <- as.numeric(c(score_AndAnd, score_AndOr, score_OrAnd, score_OrOr))
    rules_to_compare <- c(Rule_AndAnd, Rule_AndOr, Rule_OrAnd, Rule_OrOr)

    comparison_df <- data.frame(scores = scores_to_compare, rules = rules_to_compare)
    comparison_df <- comparison_df[order(-comparison_df$scores), ]

    # Update best_info_env with the top max_trees scores and rules
    for (i in 1:min(nrow(comparison_df), max_trees)) {
      update_best_info(score = comparison_df$scores[i], rule_desc = comparison_df$rules[i], best_info_env = best_info_env, internal_temperature)
    }

    best_trees_evaluated_list <- evaluate_all_rules(rules = best_info_env$best_rule_paths, data = data)
    score_merged_trees <- compute_merge_trees(rules = best_info_env$best_rule_paths, data = best_trees_evaluated_list, true_labels = data[[outcome]])
  }

  # Initialize list to store subtrees
  subtrees <- list()

  merged_trees_score <- c(score_merged_trees$F1_Score, unlist(parent_scores))
  rules_merged_trees <- c(score_merged_trees$Rule, unlist(parent_rule_names))

  subtrees$merged_trees <- create_subtree(scores = merged_trees_score, rules = rules_merged_trees,
                                          parent_scores = parent_scores, parent_rules = parent_rules,
                                          parent_rule_names = parent_rule_names, internal_temperature,
                                          temperatures, temp_index_env, data, outcome, columns, max_operators,
                                          current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

  # Create subtrees for each type
  if (length(parent_rules) == 0) {  # No previous rule
    subtrees$And <- create_subtree(scores = score_And, rules = RuleAnd,
                                   parent_scores = parent_scores, parent_rules = parent_rules,
                                   parent_rule_names = parent_rule_names, internal_temperature,
                                   temperatures, temp_index_env, data, outcome, columns, max_operators,
                                   current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$Or <- create_subtree(scores = score_Or, rules = RuleOr,
                                  parent_scores = parent_scores, parent_rules = parent_rules,
                                  parent_rule_names = parent_rule_names, internal_temperature,
                                  temperatures, temp_index_env, data, outcome, columns, max_operators,
                                  current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
  } else {
    subtrees$indiv_ruleAnd <- create_subtree(scores = score_indiv_ruleAnd, rules = Rule_indiv_And,
                                             parent_scores = parent_scores, parent_rules = parent_rules,
                                             parent_rule_names = parent_rule_names, internal_temperature,
                                             temperatures, temp_index_env, data, outcome, columns, max_operators,
                                             current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$indiv_ruleOr <- create_subtree(scores = score_indiv_ruleOr, rules = Rule_indiv_Or,
                                            parent_scores = parent_scores, parent_rules = parent_rules,
                                            parent_rule_names = parent_rule_names, internal_temperature,
                                            temperatures, temp_index_env, data, outcome, columns, max_operators,
                                            current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$AndAnd <- create_subtree(scores = score_AndAnd, rules = Rule_AndAnd,
                                      parent_scores = parent_scores, parent_rules = parent_rules,
                                      parent_rule_names = parent_rule_names, internal_temperature,
                                      temperatures, temp_index_env, data, outcome, columns, max_operators,
                                      current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$AndOr <- create_subtree(scores = score_AndOr, rules = Rule_AndOr,
                                     parent_scores = parent_scores, parent_rules = parent_rules,
                                     parent_rule_names = parent_rule_names, internal_temperature,
                                     temperatures, temp_index_env, data, outcome, columns, max_operators,
                                     current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$OrAnd <- create_subtree(scores = score_OrAnd, rules = Rule_OrAnd,
                                     parent_scores = parent_scores, parent_rules = parent_rules,
                                     parent_rule_names = parent_rule_names, internal_temperature,
                                     temperatures, temp_index_env, data, outcome, columns, max_operators,
                                     current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)

    subtrees$OrOr <- create_subtree(scores = score_OrOr, rules = Rule_OrOr,
                                    parent_scores = parent_scores, parent_rules = parent_rules,
                                    parent_rule_names = parent_rule_names, internal_temperature,
                                    temperatures, temp_index_env, data, outcome, columns, max_operators,
                                    current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees)
  }

  return(list(
    tree = list(subtrees = subtrees, best_scores = best_info_env$best_scores, best_rule_paths = best_info_env$best_rule_paths),
    best_scores = best_info_env$best_scores,
    best_rule_paths = best_info_env$best_rule_paths
  ))
}

