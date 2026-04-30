#' Update Membership Matrix
#'
#' Updates the fuzzy membership matrix (U)
#' based on distances between observations
#' and cluster prototypes.
#'
#' @param fuzzy_data List of TFN matrices.
#' @param prototypes List of cluster prototypes.
#' @param wc Modal weight.
#' @param ws Spread weight.
#' @param m Fuzzifier parameter.
#'
#' @return Updated membership matrix U.
#'
#' @keywords internal
update_membership <- function(
    fuzzy_data,
    prototypes,
    wc,
    ws,
    m = 2
) {

  n <- length(fuzzy_data)
  k <- length(prototypes)

  U <- matrix(0, n, k)

  exponent <- 1 / (m - 1)

  for (i in seq_len(n)) {

    dist_vec <- sapply(
      seq_len(k),
      function(g) {

        d <- fuzzy_distance(
          fuzzy_data[[i]],
          prototypes[[g]],
          wc,
          ws
        )

        if (d == 0) d <- 1e-10

        return(d)

      }
    )

    for (g in seq_len(k)) {

      U[i, g] <- 1 /
        sum(
          (dist_vec[g] / dist_vec)^exponent
        )

    }

  }

  return(U)

}
