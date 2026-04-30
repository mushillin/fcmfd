#' Compute Fuzzy Distance Between TFNs
#'
#' Computes the distance between two triangular fuzzy
#' number (TFN) matrices using weighted Euclidean distance.
#'
#' The distance accounts for differences in modal values
#' and spreads, controlled by weights wc and ws.
#'
#' @param x A TFN matrix (observation).
#' @param h A TFN matrix (prototype).
#' @param wc Numeric. Weight for modal component.
#' @param ws Numeric. Weight for spread components.
#'
#' @return A numeric distance value.
#'
#' @keywords internal
fuzzy_distance <- function(
    x,
    h,
    wc,
    ws
) {

  # Validate dimensions

  if (!all(dim(x) == dim(h))) {
    stop("Matrices 'x' and 'h' must have identical dimensions.")
  }

  p <- nrow(x)

  distance <- 0

  # Loop over variables

  for (j in seq_len(p)) {

    distance <- distance +

      wc^2 * (x[j, 2] - h[j, 2])^2 +

      ws^2 * (
        (x[j, 1] - h[j, 1])^2 +
          (x[j, 3] - h[j, 3])^2
      )

  }

  return(distance)

}
