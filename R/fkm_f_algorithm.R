#' Fuzzy K-Means for TFN Data
#'
#' Core iterative algorithm for fuzzy clustering
#' using triangular fuzzy numbers.
#'
#' @param fuzzy_data List of TFN matrices.
#' @param k Number of clusters.
#' @param m Fuzzifier parameter.
#' @param epsilon Convergence tolerance.
#' @param max_iter Maximum iterations.
#'
#' @return List containing clustering results.
#'
#' @keywords internal
fkm_f_algorithm <- function(
    fuzzy_data,
    k,
    m = 2,
    epsilon = 1e-6,
    max_iter = 1000
) {

  n <- length(fuzzy_data)

  # --------------------------------
  # Initialize membership matrix
  # --------------------------------

  U <- initialize_U(n, k)

  # Initial weights

  wc <- 0.5
  ws <- 0.5

  iter <- 1

  repeat {

    U_old <- U

    # --------------------------------
    # Step 1 — Update prototypes
    # --------------------------------

    prototypes <- compute_prototypes(
      fuzzy_data,
      U,
      m
    )

    # --------------------------------
    # Step 2 — Update weights
    # --------------------------------

    weights <- compute_weights(
      fuzzy_data,
      prototypes,
      U,
      m
    )

    wc <- weights$wc
    ws <- weights$ws

    # --------------------------------
    # Step 3 — Update membership
    # --------------------------------

    U <- update_membership(
      fuzzy_data,
      prototypes,
      wc,
      ws,
      m
    )

    # --------------------------------
    # Step 4 — Check convergence
    # --------------------------------

    if (check_convergence(
      U_old,
      U,
      epsilon
    )) {
      break
    }

    # --------------------------------
    # Step 5 — Max iteration control
    # --------------------------------

    if (iter >= max_iter) {

      warning(
        "Maximum number of iterations reached before convergence."
      )

      break

    }

    iter <- iter + 1

  }

  return(list(

    U = U,

    prototypes = prototypes,

    wc = wc,

    ws = ws,

    iterations = iter

  ))

}
