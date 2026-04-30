#' Compute Cluster Prototypes
#'
#' Computes fuzzy cluster prototypes (centroids)
#' using membership degrees and triangular fuzzy numbers.
#'
#' Each prototype is calculated as a weighted average
#' of fuzzy observations using membership degrees raised
#' to the fuzzifier parameter m.
#'
#' @param fuzzy_data A list of TFN matrices.
#' @param U Membership matrix.
#' @param m Numeric. Fuzzifier parameter (default = 2).
#'
#' @return A list of prototype matrices.
#'
#' @keywords internal
compute_prototypes <- function(
    fuzzy_data,
    U,
    m = 2
) {

  # Get dimensions

  n <- length(fuzzy_data)
  k <- ncol(U)
  p <- nrow(fuzzy_data[[1]])

  # Initialize prototypes

  prototypes <- vector("list", k)

  # Loop over clusters

  for (g in seq_len(k)) {

    proto <- matrix(
      NA_real_,
      nrow = p,
      ncol = 3
    )

    colnames(proto) <- c("l", "c", "r")

    # Loop over variables

    for (j in seq_len(p)) {

      numerator <- c(0, 0, 0)
      denominator <- 0

      # Loop over observations

      for (i in seq_len(n)) {

        u_m <- U[i, g]^m

        x <- fuzzy_data[[i]][j, ]

        numerator <- numerator + u_m * x

        denominator <- denominator + u_m

      }

      proto[j, ] <- numerator / denominator

    }

    prototypes[[g]] <- proto

  }

  return(prototypes)

}
