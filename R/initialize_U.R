#' Initialize Membership Matrix
#'
#' Generates an initial fuzzy membership matrix (U)
#' with random values and normalizes each row so
#' that memberships sum to 1.
#'
#' @param n Integer. Number of observations.
#' @param k Integer. Number of clusters.
#'
#' @return A numeric matrix of size n × k representing
#' initial membership degrees.
#'
#' @keywords internal
initialize_U <- function(n, k) {

  # Validate inputs

  if (!is.numeric(n) || n <= 0) {
    stop("'n' must be a positive integer.")
  }

  if (!is.numeric(k) || k <= 0) {
    stop("'k' must be a positive integer.")
  }

  # Create random membership matrix

  U <- matrix(
    runif(n * k),
    nrow = n,
    ncol = k
  )

  # Normalize rows (sum = 1)

  U <- U / rowSums(U)

  return(U)

}
