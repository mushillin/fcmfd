#' Compute Xie-Beni Cluster Validity Index
#'
#' Computes the Xie-Beni (XB) index used to evaluate
#' the compactness and separation of fuzzy clusters.
#'
#' The Xie-Beni index is defined as:
#'
#' \deqn{
#' XB =
#' \frac{
#' \sum_{i=1}^{n}
#' \sum_{g=1}^{k}
#' u_{ig}^{m} d^{2}(x_i, h_g)
#' }{
#' n \min_{g \ne g'}
#' d^{2}(h_g, h_{g'})
#' }
#' }
#'
#' Lower values indicate better clustering performance.
#'
#' @param fuzzy_data List of triangular fuzzy numbers
#'   representing the dataset.
#'
#' @param U Membership matrix (n x k).
#'
#' @param prototypes List of cluster prototypes.
#'
#' @param wc Modal weight.
#'
#' @param ws Spread weight.
#'
#' @param m Fuzzifier parameter.
#'
#' @return Numeric value representing the
#' Xie-Beni index.
#'
#' @keywords internal
compute_xie_beni_index <- function(
    fuzzy_data,
    U,
    prototypes,
    wc,
    ws,
    m = 2
) {

  n <- length(fuzzy_data)
  k <- length(prototypes)

  if (k < 2) {
    stop("Xie-Beni index requires at least two clusters.")
  }

  # ----------------------------------------
  # NUMERATOR
  # ----------------------------------------

  numerator <- 0

  for (i in seq_len(n)) {

    x <- fuzzy_data[[i]]

    for (g in seq_len(k)) {

      u_m <- U[i, g]^m

      d2 <- fuzzy_distance(
        x,
        prototypes[[g]],
        wc,
        ws
      )

      numerator <- numerator +
        u_m * d2

    }

  }

  # ----------------------------------------
  # DENOMINATOR
  # ----------------------------------------

  min_dist <- Inf

  for (g in seq_len(k - 1)) {

    for (gp in (g + 1):k) {

      d2 <- fuzzy_distance(
        prototypes[[g]],
        prototypes[[gp]],
        wc,
        ws
      )

      if (d2 < min_dist) {

        min_dist <- d2

      }

    }

  }

  # Numerical protection

  if (min_dist < .Machine$double.eps) {

    min_dist <- .Machine$double.eps

  }

  denominator <- n * min_dist

  xb <- numerator / denominator

  return(xb)

}
