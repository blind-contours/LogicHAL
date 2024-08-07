#' Compute Temperature Schedule for Simulated Annealing
#'
#' This function computes the temperature schedule for simulated annealing based on the specified type of cooling schedule.
#'
#' @param max_iterations Integer. The maximum number of iterations for the annealing process.
#' @param max_temperature Numeric. The starting (maximum) temperature for the cooling schedule.
#' @param min_temperature Numeric. The minimum temperature for the cooling schedule.
#' @param type Character. The type of cooling schedule to use. Options are "logarithmic", "geometric", or "triangular". Defaults to "logarithmic".
#'
#' @return A numeric vector of length `max_iterations` containing the temperature schedule.
#'
#' @details
#' The function supports three types of cooling schedules:
#' - `logarithmic`: The temperature decreases logarithmically, following the formula \code{T(i) = max_temperature / log(i + 1)}.
#' - `geometric`: The temperature decreases geometrically, following the formula \code{T(i) = max_temperature * alpha^i}, where `alpha` is set to 0.95.
#' - `triangular`: The temperature first decreases linearly to the minimum temperature, then increases back to the maximum temperature, forming a triangular shape.
#'
#' @examples
#' # Logarithmic cooling schedule
#' log_schedule <- compute_temperature_schedule(100, 1000, 1, "logarithmic")
#'
#' # Geometric cooling schedule
#' geo_schedule <- compute_temperature_schedule(100, 1000, 1, "geometric")
#'
#' # Triangular cooling schedule
#' tri_schedule <- compute_temperature_schedule(100, 1000, 1, "triangular")
#'
#' @export
compute_temperature_schedule <- function(max_iterations, max_temperature, min_temperature, type = "logarithmic") {
  if (type == "logarithmic") {
    return(sapply(1:max_iterations, function(i) {
      max_temperature / log(i + 1)
    }))
  } else if (type == "geometric") {
    alpha <- 0.95 # Example alpha for geometric cooling
    return(sapply(1:max_iterations, function(i) {
      max_temperature * alpha^i
    }))
  } else if (type == "triangular") {
    mid_point <- ceiling(max_iterations / 2)
    first_half <- sapply(1:mid_point, function(i) {
      max_temperature - (i - 1) * ((max_temperature - min_temperature) / (mid_point - 1))
    })
    second_half <- sapply(1:(max_iterations - mid_point), function(i) {
      min_temperature + (i - 1) * ((max_temperature - min_temperature) / (max_iterations - mid_point - 1))
    })
    return(c(first_half, second_half))
  } else if (type == "exponential") {
    return(max_temperature * exp(seq(log(1), log(min_temperature / max_temperature), length.out = max_iterations)))
  } else {
    stop("Unknown cooling schedule type")
  }
}
