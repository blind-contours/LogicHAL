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
#' Update the Best Scores and Rule Paths in the Best Info Environment
#'
#' This function updates the best scores, rule paths, and complexities in the `best_info_env` environment.
#' It compares the given score and rule description with the current best scores and updates the environment
#' if the new score is better or ties with an existing score but has a lower complexity.
#'
#' @param score A numeric value representing the score of the current rule.
#' @param rule_desc A character string describing the current rule.
#' @param best_info_env An environment containing the best scores, rule paths, and complexities.
#'                      It should have the elements `best_scores`, `best_rule_paths`, and `best_complexities`.
#'
#' @return Updates the `best_info_env` environment in place.
#'
#' @examples
#' # Create an example best_info_env environment
#' best_info_env <- new.env()
#' best_info_env$best_scores <- c(0.8, 0.75, 0.70)
#' best_info_env$best_rule_paths <- list("rule1", "rule2", "rule3")
#' best_info_env$best_complexities <- c(2, 3, 4)
#'
#' # Update the best info environment with a new score and rule description
#' update_best_info(0.78, "new_rule", best_info_env)
#'
#' @export
update_best_info <- function(score, rule_desc, best_info_env) {
  current_best_scores <- best_info_env$best_scores
  current_best_rules <- best_info_env$best_rule_paths
  current_best_complexities <- best_info_env$best_complexities
  new_complexity <- count_logical_operators(rule_desc)

  if (score >= min(current_best_scores)) {
    if (score %in% current_best_scores) {
      tied_indices <- which(current_best_scores == score)
      for (index in tied_indices) {
        current_rule <- current_best_rules[[index]]
        current_complexity <- current_best_complexities[index]
        if (new_complexity < current_complexity) {
          current_best_rules[[index]] <- rule_desc
          current_best_complexities[index] <- new_complexity
          break
        }
      }
    } else {
      min_index <- which.min(current_best_scores)
      current_best_scores[min_index] <- score
      current_best_rules[min_index] <- rule_desc
      current_best_complexities[min_index] <- new_complexity
    }

    sorted_indices <- order(current_best_scores, -current_best_complexities, decreasing = TRUE)
    best_info_env$best_scores <- current_best_scores[sorted_indices]
    best_info_env$best_rule_paths <- current_best_rules[sorted_indices]
    best_info_env$best_complexities <- current_best_complexities[sorted_indices]
  }
}
#' Create Subtrees Based on Scores and Rules
#'
#' This function creates subtrees by selecting rules based on their scores using a Boltzmann distribution.
#' It normalizes the scores, calculates selection probabilities, and selects indices to form subtrees.
#' The selected rules are then used to recursively build logic trees.
#'
#' @param scores A numeric vector of scores for the current rules.
#' @param rules A character vector of rule descriptions corresponding to the scores.
#' @param parent_score A numeric value representing the score of the parent rule.
#' @param parent_rule A logical vector indicating whether each observation satisfies the parent rule.
#' @param parent_rule_name A character string describing the parent rule.
#' @param internal_temperature A numeric value for the internal temperature used in the Boltzmann distribution.
#' @param external_temperature A numeric value for the external temperature, affecting the recursion depth.
#' @param data A data frame containing the dataset.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param columns A character vector of feature names to be considered for logical interactions.
#' @param max_operators An integer specifying the maximum number of logic operators for the rules.
#' @param current_operators An integer specifying the current number of logic operators in the rule.
#' @param best_info_env An environment containing the best scores, rule paths, and complexities.
#' @param two_way_logic_roots A list containing precomputed scores and rules for two-way logic interactions.
#' @param beam_width An integer specifying the number of indices to sample (beam width).
#'
#' @return A list of subtrees created based on the selected rules and their scores.
#'
#' @examples
#' # Assuming `data`, `columns`, `outcome`, `best_info_env`, and `two_way_logic_roots` are predefined
#' scores <- c(0.8, 0.75, 0.7)
#' rules <- c("rule1", "rule2", "rule3")
#' parent_score <- 0.85
#' parent_rule <- rep(TRUE, nrow(data))
#' parent_rule_name <- "parent_rule"
#' internal_temperature <- 1.0
#' external_temperature <- 1.0
#' max_operators <- 3
#' current_operators <- 0
#' beam_width <- 3
#'
#' subtrees <- create_subtree(scores, rules, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width)
#'
#' @export
create_subtree <- function(scores, rules, parent_score, parent_rule, parent_rule_name, internal_temperature, external_temperature, data, outcome, columns, max_operators, current_operators, best_info_env, two_way_logic_roots, beam_width, max_trees) {
  if (any(scores != 0, na.rm = TRUE)) {
    # Normalize scores to prevent overflow in exponentiation
    max_score <- max(scores, na.rm = TRUE)
    normalized_scores <- scores - max_score

    # Calculate probabilities using Boltzmann distribution
    probabilities <- exp(normalized_scores / internal_temperature)
    probabilities <- probabilities / sum(probabilities, na.rm = TRUE)

    selected_indices <- sample(1:length(scores), size = beam_width, prob = probabilities, replace = TRUE)

    subtree_list <- list()
    for (selected_index in selected_indices) {
      selected_score <- scores[selected_index]
      selected_rule_desc <- rules[selected_index]

      # Try-catch block for debugging
      selected_rule <- tryCatch({
        data %>%
          mutate(rule = eval(parse(text = selected_rule_desc))) %>%
          pull(rule)
      }, error = function(e) {
        cat("Error in evaluating rule:", selected_rule_desc, "\n")
        cat("Error message:", e$message, "\n")
        browser() # Pause execution for debugging
      })

      update_best_info(score = selected_score, rule_desc = selected_rule_desc, best_info_env)

      subtree_result <- recursive_build_logic_tree(
        data = data, outcome = outcome, columns = columns,
        max_operators = max_operators, current_operators = current_operators + count_logical_operators(selected_rule_desc),
        parent_score = selected_score, previous_rule = selected_rule,
        previous_rule_name = selected_rule_desc, best_info_env = best_info_env,
        external_temperature = external_temperature, two_way_logic_roots = two_way_logic_roots,
        beam_width = beam_width,
        max_trees = max_trees
      )

      subtree_list <- c(subtree_list, list(list(
        split = selected_rule_desc,
        score = round(selected_score, 3),
        subtree = subtree_result$tree
      )))
    }
    return(subtree_list)
  }
  return(NULL)
}





