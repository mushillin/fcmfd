#' Print Method for fcmTFN Objects
#'
#' Displays a concise summary of a fitted
#' fuzzy C-means model using triangular fuzzy numbers.
#'
#' @param x An object of class "fcmTFN".
#' @param ... Additional arguments (not used).
#'
#' @return The input object (invisibly).
#'
#' @export

print_fcmTFN <- function(x, ...) {

  if (!inherits(x, "fcmTFN")) {

    stop("Object must be of class 'fcmTFN'.")

  }

  cat("\n")
  cat("Fuzzy C-Means Clustering for TFN\n")
  cat("---------------------------------\n\n")

  cat("Optimal number of clusters : ",
      x$best_k,
      "\n",
      sep = ""
  )

  cat("Iterations                 : ",
      x$iterations,
      "\n\n",
      sep = ""
  )

  cat("Weights:\n")

  cat("wc = ",
      round(x$wc, 6),
      "\n",
      sep = ""
  )

  cat("ws = ",
      round(x$ws, 6),
      "\n\n",
      sep = ""
  )

  cat("Scale configuration:\n")

  cat("Type   : ",
      x$type,
      "\n",
      sep = ""
  )

  cat("Option : ",
      x$option,
      "\n",
      sep = ""
  )

  cat("\n")

  invisible(x)

}
