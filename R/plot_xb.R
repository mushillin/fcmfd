#' Plot Xie-Beni Index
#'
#' Plots the Xie-Beni validity index
#' across candidate numbers of clusters.
#'
#' @importFrom graphics axis box lines par segments text points
#'
#' @param object An object of class "fcmTFN".
#' @param mark_optimal Logical. Whether to highlight the optimal k.
#' @param type Character string indicating the plotting style
#'   (default = "b").
#' @param show_title Logical. If TRUE (default),
#'   displays the plot title.
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns NULL.
#'
#' @export

plot_xb <- function(
    object,
    mark_optimal = TRUE,
    type = "b",
    show_title = TRUE,
    ...
) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  # --------------------------------
  # Prepare data
  # --------------------------------

  k_vals <- object$k_values

  xb_vals <- object$xb_values

  if (length(k_vals) != length(xb_vals)) {

    stop("Mismatch between k_values and xb_values.")

  }

  # --------------------------------
  # Title and margins
  # --------------------------------

  plot_title <- if (show_title) {

    "Cluster Validation: Xie-Beni Index"

  } else {

    ""

  }

  top_margin <- if (show_title) 4 else 1

  op <- par(

    mar = c(5, 4, top_margin, 2) + 0.1

  )

  on.exit(
    par(op)
  )

  # --------------------------------
  # Plot
  # --------------------------------

  plot(

    k_vals,

    xb_vals,

    type = type,

    xlab = "Number of clusters (k)",

    ylab = "Xie-Beni Index",

    main = plot_title,

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
