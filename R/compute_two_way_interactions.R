#' Compute Two-Way Interactions in Parallel
#'
#' This function computes two-way logical interactions (AND/OR) among features in parallel
#' to optimize computational efficiency. The function uses multiple cores to distribute the
#' computation of F1 scores for all pairwise logical interactions.
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param columns A character vector of feature names to be considered for logical interactions.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param num_cores Integer. The number of cores to use for parallel processing. Default is one less than the number of available cores.
#'
#' @return A list of results from each core, each containing the computed F1 scores for the two-way interactions.
#'
#' @examples
#' # Assuming 'df' is your data frame and 'outcome' is the name of your outcome variable
#' columns <- colnames(df)[grep("^KRFP", colnames(df))]
#' results <- compute_two_way_interactions(df, columns, "outcome", num_cores = 4)
#'
#' @importFrom parallel makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @importFrom Rcpp sourceCpp
#' @export
# Main function to compute pairwise logic interactions in parallel
compute_pairwise_logic_interactions_parallel <- function(data, columns, outcome, num_cores = detectCores() - 1) {
  # Generate all unique pairwise combinations of columns
  pairs <- combn(columns, 2, simplify = FALSE)

  # Split pairs into chunks based on the number of cores
  chunk_size <- ceiling(length(pairs) / num_cores)
  chunks <- split(pairs, ceiling(seq_along(pairs) / chunk_size))

  cl <- makeCluster(num_cores)

  # Ensure necessary packages and functions are loaded on each worker
  clusterEvalQ(cl, {
    library(Rcpp)
    sourceCpp("src/computePairwiseLogicInteractions.cpp")
  })

  # Export required data and functions to the cluster
  clusterExport(cl, c("data", "columns", "outcome", "chunks"), envir = environment())

  # Compute pairwise logic interactions in parallel
  results <- parLapply(cl, chunks, function(chunk) {
    computePairwiseLogicInteractions(data, chunk, data[[outcome]])
  })

  stopCluster(cl)

  # Combine results from all chunks
  combined_results <- list(F1And = numeric(), F1Or = numeric(), RuleAnd = character(), RuleOr = character())

  for (result in results) {
    combined_results$F1And <- c(combined_results$F1And, result$F1And)
    combined_results$F1Or <- c(combined_results$F1Or, result$F1Or)
    combined_results$RuleAnd <- c(combined_results$RuleAnd, result$RuleAnd)
    combined_results$RuleOr <- c(combined_results$RuleOr, result$RuleOr)
  }

  return(combined_results)
}
