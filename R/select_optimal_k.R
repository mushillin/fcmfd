#' Select Optimal Number of Clusters
#'
#' Runs the fuzzy clustering algorithm for
#' multiple candidate values of k and selects
#' the optimal one using the Xie-Beni index.
#'
#' @param fuzzy_data List of TFN matrices.
#' @param k_values Numeric vector of candidate
#' cluster numbers.
#' @param m Fuzzifier parameter.
#' @param epsilon Convergence tolerance.
#' @param max_iter Maximum number of iterations.
#' @param verbose Logical; if TRUE,
#' prints progress messages.
#'
#' @return A list containing:
#' \itemize{
#' \item best_k Optimal number of clusters
#' \item xb_values Xie-Beni values
#' \item results List of clustering results
#' }
#'
#' @keywords internal

select_optimal_k <- function(
    fuzzy_data,
    k_values,
    m = 2,
    epsilon = 1e-6,
    max_iter = 100,
    verbose = TRUE
) {

  xb_values <- numeric(
    length(k_values)
  )

  results_list <- list()

  for (i in seq_along(k_values)) {

    k <- k_values[i]

    if (verbose) {
      cat(
        "Running k =",
        k,
        "\n"
      )
    }

    # Run clustering

    result <- fkm_f_algorithm(
      fuzzy_data,
      k,
      m,
      epsilon,
      max_iter
    )

    # Compute XB index

    xb <- compute_xie_beni_index(
      fuzzy_data,
      result$U,
      result$prototypes,
      result$wc,
      result$ws,
      m
    )

    xb_values[i] <- xb

    results_list[[i]] <- result

  }

  best_k <- k_values[
    which.min(xb_values)
  ]

  return(list(

    best_k = best_k,

    xb_values = xb_values,

    results = results_list

  ))

}
