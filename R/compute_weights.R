#' Compute Adaptive Weights
#'
#' Computes adaptive weights for modal (wc)
#' and spread (ws) components based on
#' fuzzy distances and membership degrees.
#'
#' @param fuzzy_data List of TFN matrices.
#' @param prototypes List of prototype matrices.
#' @param U Membership matrix.
#' @param m Fuzzifier parameter.
#'
#' @return A list with wc and ws.
#'
#' @keywords internal
compute_weights <- function(
    fuzzy_data,
    prototypes,
    U,
    m = 2
) {

  n <- length(fuzzy_data)
  k <- length(prototypes)

  numerator <- 0
  denominator <- 0

  for (i in seq_len(n)) {

    x <- fuzzy_data[[i]]

    p <- nrow(x)

    for (g in seq_len(k)) {

      h <- prototypes[[g]]

      u_m <- U[i, g]^m

      d_center <- 0
      d_spread <- 0

      for (j in seq_len(p)) {

        d_center <- d_center +
          (x[j, 2] - h[j, 2])^2

        d_spread <- d_spread +
          (x[j, 1] - h[j, 1])^2 +
          (x[j, 3] - h[j, 3])^2

      }

      numerator <- numerator +
        u_m * d_spread

      denominator <- denominator +
        u_m * (d_center + d_spread)

    }

  }

  # Avoid division by zero

  if (denominator == 0) {
    stop("Denominator is zero while computing weights.")
  }

  wc <- numerator / denominator
  ws <- 1 - wc

  return(list(
    wc = wc,
    ws = ws
  ))

}
