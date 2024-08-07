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
update_best_info <- function(score, rule_desc, best_info_env, internal_temperature) {
  current_best_scores <- best_info_env$best_scores
  current_best_rules <- best_info_env$best_rule_paths
  current_best_complexities <- best_info_env$best_complexities
  new_complexity <- count_logical_operators(rule_desc)
  current_temperatures <- best_info_env$temperature

  if (score >= min(current_best_scores)) {
    if (score %in% current_best_scores) {
      tied_indices <- which(current_best_scores == score)
      for (index in tied_indices) {
        current_rule <- current_best_rules[[index]]
        current_complexity <- current_best_complexities[index]
        if (new_complexity < current_complexity) {
          current_best_rules[[index]] <- rule_desc
          current_best_complexities[index] <- new_complexity
          current_temperatures[index] <- internal_temperature
          break
        }
      }
    } else {
      min_index <- which.min(current_best_scores)
      current_best_scores[min_index] <- score
      current_best_rules[min_index] <- rule_desc
      current_best_complexities[min_index] <- new_complexity
      current_temperatures[min_index] <- internal_temperature
    }

    sorted_indices <- order(current_best_scores, -current_best_complexities, decreasing = TRUE)
    best_info_env$best_scores <- current_best_scores[sorted_indices]
    best_info_env$best_rule_paths <- current_best_rules[sorted_indices]
    best_info_env$best_complexities <- current_best_complexities[sorted_indices]
    best_info_env$temperature <- current_temperatures[sorted_indices]

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
create_subtree <- function(scores, rules, parent_scores, parent_rules, parent_rule_names, internal_temperature,
                           temperatures, temp_index_env, data, outcome, columns, max_operators, current_operators,
                           best_info_env, two_way_logic_roots, beam_width, max_trees) {
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

      update_best_info(score = selected_score, rule_desc = selected_rule_desc, best_info_env, internal_temperature)

      # Update parent rules and scores
      parent_rules <- append(parent_rules, list(selected_rule))
      parent_rule_names <- append(parent_rule_names, selected_rule_desc)
      parent_scores <- append(parent_scores, selected_score)


      subtree_result <- recursive_build_logic_tree(
        data = data,
        outcome = outcome,
        columns = columns,
        max_operators = max_operators,
        current_operators = current_operators + count_logical_operators(selected_rule_desc),
        parent_scores = parent_scores,
        parent_rules = parent_rules,
        parent_rule_names = parent_rule_names,
        best_info_env = best_info_env,
        temperatures = temperatures,
        temp_index_env = temp_index_env,
        two_way_logic_roots = two_way_logic_roots,
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

#' Evaluate a List of Rules on Data
#'
#' This function evaluates a list of logical rules on a given dataset and stores the results in a DataFrame.
#'
#' @param rules A character vector containing the logical rules to be evaluated.
#' @param data A data frame containing the data on which the rules will be evaluated.
#' @return A data frame where each column corresponds to the evaluated result (0/1) of each rule on the data.
#' @examples
#' rules <- c("(KRFP1536 | KRFP4081)", "((KRFP1536 == 1 | KRFP4081 == 1) & (KRFP3752 | Not_KRFP138))")
#' data <- data.frame(KRFP1536 = c(0, 1), KRFP4081 = c(1, 0), KRFP3752 = c(0, 1), Not_KRFP138 = c(1, 0))
#' evaluate_all_rules(rules, data)
#' @export
evaluate_all_rules <- function(rules, data) {
  results <- data.frame(matrix(ncol = length(rules), nrow = nrow(data)))
  colnames(results) <- rules

  for (i in seq_along(rules)) {
    results[, i] <- eval(parse(text = rules[[i]]), envir = data)
  }

  return(results)
}

#' Calculate Swap Probability
#'
#' This function calculates the probability of swapping two states based on their scores and temperatures.
#' It uses the Metropolis criterion to determine the swap probability.
#'
#' @param score1 Numeric. The score of the first state.
#' @param score2 Numeric. The score of the second state.
#' @param temp1 Numeric. The temperature associated with the first state.
#' @param temp2 Numeric. The temperature associated with the second state.
#'
#' @return A numeric value between 0 and 1 representing the probability of swapping the two states.
#' The probability is calculated using the formula \eqn{\exp((score2 - score1) / (temp_high - temp_low))},
#' where \eqn{temp_high} and \eqn{temp_low} are the higher and lower temperatures of the two states, respectively.
#'
#' @examples
#' # Example usage:
#' calculate_swap_probability(score1 = 0.8, score2 = 0.9, temp1 = 0.5, temp2 = 0.6)
#'
#' @export
calculate_swap_probability <- function(score1, score2, temp1, temp2) {
  delta_score <- score2 - score1
  # Ensure the temperatures are ordered correctly
  temp_high <- max(temp1, temp2)
  temp_low <- min(temp1, temp2)
  return(min(1, exp(delta_score / (temp_high - temp_low))))
}

#' Fit a Lasso Regression Model
#'
#' This function fits a Lasso regression model using cross-validated \code{glmnet}.
#'
#' @param features A matrix of predictor variables. Each column represents a predictor, and each row represents an observation.
#' @param outcome A vector of response variables. The length of \code{outcome} should match the number of rows in \code{features}.
#' @param family A character string specifying the error distribution and link function to be used in the model. Options include \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, etc.
#'
#' @return A \code{cv.glmnet} model object containing the fitted Lasso regression model.
#'
#' @examples
#' \dontrun{
#' # Example with gaussian family
#' features <- matrix(rnorm(100*10), 100, 10)
#' outcome <- rnorm(100)
#' lasso_model <- fit_lasso(features, outcome, family = "gaussian")
#'
#' # Example with binomial family
#' features <- matrix(rnorm(100*10), 100, 10)
#' outcome <- sample(c(0, 1), 100, replace = TRUE)
#' lasso_model <- fit_lasso(features, outcome, family = "binomial")
#' }
#'
#' @import glmnet
#' @export
fit_lasso <- function(features, outcome, family) {
  model <- cv.glmnet(as.matrix(features), outcome, family = family)
  return(model)
}

#' Calculate F1 Score
#'
#' Computes the F1 score, which is the harmonic mean of precision and recall, for a given set of predictions and actual outcomes.
#'
#' @param predictions A numeric vector of predicted values, where 1 indicates a positive prediction and 0 indicates a negative prediction.
#' @param actual A numeric vector of actual outcomes, where 1 indicates a positive outcome and 0 indicates a negative outcome.
#'
#' @return A numeric value representing the F1 score.
#'
#' @details The F1 score is calculated using the formula:
#'   \deqn{F1 = \frac{2 \times \text{precision} \times \text{recall}}{\text{precision} + \text{recall}}}
#'   where precision is the ratio of true positives to the total number of predicted positives, and recall is the ratio of true positives to the actual number of positives.
#'
#' @examples
#' # Example usage
#' predictions <- c(1, 0, 1, 1, 0)
#' actual <- c(1, 0, 0, 1, 1)
#' calculate_f1_score(predictions, actual)
#'
#' @export
calculate_f1_score <- function(predictions, actual) {
  # Calculate True Positives, False Positives, False Negatives
  tp <- sum(predictions == 1 & actual == 1)
  fp <- sum(predictions == 1 & actual == 0)
  fn <- sum(predictions == 0 & actual == 1)

  # Calculate Precision and Recall
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)

  # Calculate F1 Score
  f1_score <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)

  return(f1_score)
}



