#' Summary for fcmTFN Objects
#'
#' Displays a summary of the Fuzzy C-Means clustering
#' results for triangular fuzzy numbers.
#'
#' @param object An object of class "fcmTFN".
#' @param ... Additional arguments (not used).
#'
#' @return Prints a formatted summary of the clustering result.
#'
#' @export

summary.fcmTFN <- function(object, ...) {

  cat("\n")
  cat("Fuzzy C-Means Clustering for TFN\n")
  cat("---------------------------------\n\n")

  cat("Optimal number of clusters (k): ",
      object$best_k, "\n\n")

  cat("Weights:\n")
  cat("wc =", round(object$wc, 6), "\n")
  cat("ws =", round(object$ws, 6), "\n\n")

  cat("Iterations:", object$iterations, "\n\n")

  cat("Scale configuration:\n")
  cat("Type   :", object$type, "\n")
  cat("Option :", object$option, "\n\n")

  cat("Xie-Beni values:\n")

  for (i in seq_along(object$k_values)) {

    cat(
      "k =", object$k_values[i],
      ":",
      round(object$xb_values[i], 6),
      "\n"
    )

  }

  cat("\n")

  invisible(object)
}

