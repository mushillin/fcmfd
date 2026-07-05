#' Plot Fuzzy Dictionary
#'
#' Plots the triangular fuzzy numbers
#' defining the Likert-scale dictionary.
#'
#' @param object An object of class "fcmTFN".
#' @param show_title Logical. If TRUE (default),
#'   displays the plot title.
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns NULL.
#'
#' @export

plot_dictionary <- function(
    object,
    show_title = TRUE,
    ...
) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  if (is.null(object$dictionary)) {

    stop("Dictionary not found in object.")

  }

  dic <- do.call(
    rbind,
    object$dictionary
  )

  dic <- as.matrix(dic)

  l_vals <- dic[, 1]

  c_vals <- dic[, 2]

  r_vals <- dic[, 3]

  n <- nrow(dic)

  x_min <- min(l_vals)

  x_max <- max(r_vals)

  # --------------------------------
  # Title and margins
  # --------------------------------

  plot_title <- if (show_title) {

    "Likert scale (modal values)"

  } else {

    ""

  }

  top_margin <- if (show_title) 5 else 1

  op <- par(

    mar = c(5, 4, top_margin, 2)

  )

  on.exit(
    par(op)
  )

  # --------------------------------
  # Plot
  # --------------------------------

  plot(

    NA,

    xlim = c(
      x_min,
      x_max
    ),

    ylim = c(
      0,
      1
    ),

    xlab = "Support intervals",

    ylab = "Membership degree",

    main = plot_title,

    xaxt = "n",

    yaxt = "n",

    type = "n",

    ...

  )

  box()

  # --------------------------------
  # Y axis
  # --------------------------------

  axis(

    side = 2,

    at = c(
      0,
      1
    ),

    labels = c(
      "0",
      "1"
    ),

    las = 1

  )

  # --------------------------------
  # Triangular fuzzy numbers
  # --------------------------------

  for (i in seq_len(n)) {

    lines(

      c(
        l_vals[i],
        c_vals[i]
      ),

      c(
        0,
        1
      ),

      lwd = 0.8

    )

    lines(

      c(
        c_vals[i],
        r_vals[i]
      ),

      c(
        1,
        0
      ),

      lwd = 0.8

    )

  }

  # --------------------------------
  # Close last triangle
  # --------------------------------

  lines(

    c(
      r_vals[n],
      r_vals[n]
    ),

    c(
      0,
      1
    ),

    lwd = 0.8

  )

  usr <- par("usr")

  y_bottom <- usr[3]

  par(
    xpd = NA
  )

  tick_len <- 0.05 *
    abs(par("tcl")) *
    (usr[4] - usr[3])

  # --------------------------------
  # Bottom ticks
  # --------------------------------

  segments(

    x0 = c_vals,

    y0 = y_bottom,

    x1 = c_vals,

    y1 = y_bottom - tick_len,

    lwd = 0.8

  )

  # --------------------------------
  # Modal values
  # --------------------------------

  text(

    x = c_vals,

    y = usr[4] + 0.7 * tick_len,

    labels = c_vals,

    cex = 0.9

  )

  # --------------------------------
  # Support intervals
  # --------------------------------

  interval_labels <- paste0(

    "(",

    round(
      l_vals,
      2
    ),

    ", ",

    round(
      r_vals,
      2
    ),

    ")"

  )

  text(

    x = c_vals,

    y = y_bottom - 1.6 * tick_len,

    labels = interval_labels,

    cex = 0.8

  )

  invisible(NULL)

}
