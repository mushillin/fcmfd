#' Plot Cluster Prototypes
#'
#' Visualizes cluster prototypes as interval plots
#' using triangular fuzzy numbers (l, c, r).
#'
#' @importFrom graphics axis box lines par segments text points
#'
#' @param object An object of class "fcmTFN".
#' @param cluster Integer cluster to plot.
#' @param use_var_names Logical.
#' @param var_names Optional variable names.
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns NULL.
#'
#' @export

plot_prototypes <- function(
    object,
    cluster = 1,
    use_var_names = FALSE,
    var_names = NULL,
    ...
) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  prototypes <- object$prototypes

  k <- length(prototypes)

  if (cluster < 1 || cluster > k) {

    stop("Invalid cluster index.")

  }

  proto <- prototypes[[cluster]]

  p <- nrow(proto)

  # --------------------------------
  # Variable names
  # --------------------------------

  if (use_var_names) {

    if (is.null(var_names)) {

      stop("Provide var_names.")

    }

    if (length(var_names) != p) {

      stop("Incorrect number of variable names.")

    }

  } else {

    var_names <- paste0(
      "Var",
      seq_len(p)
    )

  }

  # --------------------------------
  # Prepare plot

  plot(
    NA,
    xlim = range(proto),
    ylim = c(1, p),
    xlab = "TFN values",
    ylab = "",
    yaxt = "n",
    main = paste(
      "Cluster",
      cluster,
      "Prototypes"
    ),
    ...
  )

  axis(
    2,
    at = seq_len(p),
    labels = var_names,
    las = 1
  )

  # --------------------------------
  # Draw TFN intervals

  for (j in seq_len(p)) {

    l <- proto[j, 1]
    c <- proto[j, 2]
    r <- proto[j, 3]

    segments(
      l,
      j,
      r,
      j,
      lwd = 2
    )

    points(
      c,
      j,
      pch = 19
    )

  }

  invisible(NULL)

}
