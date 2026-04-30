#' Plot Xie-Beni Index
#'
#' Plots the Xie-Beni validity index
#' across candidate numbers of clusters.
#'
#' @importFrom graphics axis box lines par segments text points
#'
#' @param object An object of class "fcmTFN".
#' @param mark_optimal Logical. Whether to highlight the optimal k.
#' @param type Plot type (default = "b").
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns NULL.
#'
#' @export

plot_xb <- function(
    object,
    mark_optimal = TRUE,
    type = "b",
    ...
) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  k_vals <- object$k_values
  xb_vals <- object$xb_values

  if (length(k_vals) != length(xb_vals)) {

    stop("Mismatch between k_values and xb_values.")

  }

  # --------------------------------
  # Plot XB curve
  # --------------------------------

  plot(
    k_vals,
    xb_vals,
    type = type,
    xlab = "Number of clusters (k)",
    ylab = "Xie-Beni Index",
    main = "Xie-Beni Index vs Number of Clusters",
    ...
  )

  # --------------------------------
  # Mark optimal k
  # --------------------------------

  if (mark_optimal) {

    best_k <- object$best_k

    best_index <- which(
      k_vals == best_k
    )

    points(
      k_vals[best_index],
      xb_vals[best_index],
      pch = 19
    )

    text(
      k_vals[best_index],
      xb_vals[best_index],
      labels = paste0(
        "Optimal k = ",
        best_k
      ),
      pos = 3
    )

  }

  invisible(NULL)

}
