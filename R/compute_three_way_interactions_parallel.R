#' Compute Three-Way Interactions in Parallel
#'
#' This function computes three-way logical interactions (AND/OR) among features in parallel
#' to optimize computational efficiency. The function uses multiple cores to distribute the
#' computation of F1 scores for all three-way logical interactions.
#'
#' @param data A data frame containing the features and the outcome variable.
#' @param columns A character vector of feature names to be considered for logical interactions.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param num_cores Integer. The number of cores to use for parallel processing. Default is one less than the number of available cores.
#'
#' @return A list of results from each core, each containing the computed F1 scores for the three-way interactions.
#'
#' @examples
#' # Assuming 'df' is your data frame and 'outcome' is the name of your outcome variable
#' columns <- colnames(df)[grep("^KRFP", colnames(df))]
#' results <- compute_three_way_interactions_parallel(df, columns, "outcome", num_cores = 4)
#'
#' @importFrom parallel makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @importFrom Rcpp sourceCpp
#' @export
compute_three_way_interactions_parallel <- function(data, columns, outcome, num_cores = parallel::detectCores() - 1) {
  # Generate all unique three-way combinations of columns
  triples <- combn(columns, 3, simplify = FALSE)

  # Split triples into chunks based on the number of cores
  chunk_size <- ceiling(length(triples) / num_cores)
  chunks <- split(triples, ceiling(seq_along(triples) / chunk_size))

  cl <- parallel::makeCluster(num_cores)

  # Ensure necessary packages and functions are loaded on each worker
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
    sourceCpp("src/computeThreeWayLogicInteractions.cpp")
  })

  # Export required data and functions to the cluster
  parallel::clusterExport(cl, c("data", "outcome", "chunks"), envir = environment())

  # Compute three-way logic interactions in parallel
  results <- parallel::parLapply(cl, chunks, function(chunk) {
    computeThreeWayLogicInteractions(data, chunk, data[[outcome]])
  })

  parallel::stopCluster(cl)

  # Combine results from all chunks
  combined_results <- list(
    F1_AndAndAnd = numeric(), F1_AndAndOr = numeric(), F1_AndOr = numeric(), F1_OrAnd = numeric(),
    F1_OrOr = numeric(), F1_AndOrAnd = numeric(), F1_OrAndOr = numeric(), F1_OrOrAnd = numeric(),
    Rule_AndAndAnd = character(), Rule_AndAndOr = character(), Rule_AndOr = character(),
    Rule_OrAnd = character(), Rule_OrOr = character(), Rule_AndOrAnd = character(),
    Rule_OrAndOr = character(), Rule_OrOrAnd = character()
  )

  for (result in results) {
    combined_results$F1_AndAndAnd <- c(combined_results$F1_AndAndAnd, result$F1_AndAndAnd)
    combined_results$F1_AndAndOr <- c(combined_results$F1_AndAndOr, result$F1_AndAndOr)
    combined_results$F1_AndOr <- c(combined_results$F1_AndOr, result$F1_AndOr)
    combined_results$F1_OrAnd <- c(combined_results$F1_OrAnd, result$F1_OrAnd)
    combined_results$F1_OrOr <- c(combined_results$F1_OrOr, result$F1_OrOr)
    combined_results$F1_AndOrAnd <- c(combined_results$F1_AndOrAnd, result$F1_AndOrAnd)
    combined_results$F1_OrAndOr <- c(combined_results$F1_OrAndOr, result$F1_OrAndOr)
    combined_results$F1_OrOrAnd <- c(combined_results$F1_OrOrAnd, result$F1_OrOrAnd)

    combined_results$Rule_AndAndAnd <- c(combined_results$Rule_AndAndAnd, result$Rule_AndAndAnd)
    combined_results$Rule_AndAndOr <- c(combined_results$Rule_AndAndOr, result$Rule_AndAndOr)
    combined_results$Rule_AndOr <- c(combined_results$Rule_AndOr, result$Rule_AndOr)
    combined_results$Rule_OrAnd <- c(combined_results$Rule_OrAnd, result$Rule_OrAnd)
    combined_results$Rule_OrOr <- c(combined_results$Rule_OrOr, result$Rule_OrOr)
    combined_results$Rule_AndOrAnd <- c(combined_results$Rule_AndOrAnd, result$Rule_AndOrAnd)
    combined_results$Rule_OrAndOr <- c(combined_results$Rule_OrAndOr, result$Rule_OrAndOr)
    combined_results$Rule_OrOrAnd <- c(combined_results$Rule_OrOrAnd, result$Rule_OrOrAnd)
  }

  return(combined_results)
}
