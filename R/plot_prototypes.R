#' Plot Cluster Prototypes
#'
#' Visualizes cluster prototypes as interval plots
#' using triangular fuzzy numbers (l, c, r).
#'
#' @importFrom graphics axis box legend par plot points segments
#'
#' @param object An object of class "fcmTFN".
#' @param cluster Integer cluster to plot when
#'   view = "single".
#' @param view Character string indicating the
#'   visualization mode. Either "single" or "global".
#' @param use_var_names Logical.
#' @param var_names Optional character vector of variable names.
#' @param show_title Logical. If TRUE (default),
#'   displays the plot title.
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns NULL.
#'
#' @export

plot_prototypes <- function(
    object,
    cluster = 1,
    view = c("single", "global"),
    use_var_names = FALSE,
    var_names = NULL,
    show_title = TRUE,
    ...
) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  view <- match.arg(view)

  prototypes <- object$prototypes

  k <- length(prototypes)

  p <- nrow(prototypes[[1]])

  # --------------------------------
  # Validate cluster
  # --------------------------------

  if (
    view == "single" &&
    (cluster < 1 || cluster > k)
  ) {

    stop("Invalid cluster index.")

  }

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

  # =================================
  # SINGLE VIEW
  # =================================

  if (view == "single") {

    # --------------------------------
    # Prepare data
    # --------------------------------

    proto <- prototypes[[cluster]]

    xmin <- min(proto[, 1])

    xmax <- max(proto[, 3])

    range_width <- xmax - xmin

    margin <- 0.05 * range_width

    if (range_width == 0) {

      margin <- 0.5

    }

    x_limits <- c(

      xmin - margin,

      xmax + margin

    )

    # --------------------------------
    # Title and margins
    # --------------------------------

    plot_title <- if (show_title) {

      paste(

        "Cluster",

        cluster,

        "Prototype"

      )

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

      NA,

      xlim = x_limits,

      ylim = c(1, p),

      xlab = "TFN values",

      ylab = "",

      yaxt = "n",

      main = plot_title,

      ...

    )

    axis(

      2,

      at = seq_len(p),

      labels = var_names,

      las = 1

    )

    # --------------------------------
    # Cluster prototype
    # --------------------------------

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

    box()

    return(
      invisible(NULL)
    )

  }

  # =================================
  # GLOBAL VIEW
  # =================================
  # --------------------------------
  # Prepare data
  # --------------------------------

  all_proto <- do.call(
    rbind,
    prototypes
  )

  xmin <- min(all_proto[, 1])

  xmax <- max(all_proto[, 3])

  range_width <- xmax - xmin

  margin <- 0.05 * range_width

  if (range_width == 0) {

    margin <- 0.5

  }

  x_limits <- c(

    xmin - margin,

    xmax + margin

  )

  cols <- c(

    "#1B9E77",
    "#377EB8",
    "#E41A1C",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#4DAF4A"

  )

  cols <- rep(

    cols,

    length.out = k

  )

  # --------------------------------
  # Title and margins
  # --------------------------------

  plot_title <- if (show_title) {

    "Cluster Prototypes"

  } else {

    ""

  }

  top_margin <- if (show_title) 4 else 1

  op <- par(

    mar = c(5, 4, top_margin, 8) + 0.1,

    xpd = TRUE

  )

  on.exit(
    par(op)
  )

  # --------------------------------
  # Plot
  # --------------------------------

  plot(

    NA,

    xlim = x_limits,

    ylim = c(1, p),

    xlab = "TFN values",

    ylab = "",

    yaxt = "n",

    main = plot_title,

    ...

  )

  axis(

    2,

    at = seq_len(p),

    labels = var_names,

    las = 1

  )

  # --------------------------------
  # Cluster prototypes
  # --------------------------------

  for (g in seq_len(k)) {

    proto <- prototypes[[g]]

    for (j in seq_len(p)) {

      l <- proto[j, 1]

      c <- proto[j, 2]

      r <- proto[j, 3]

      segments(

        l,
        j,

        r,
        j,

        lwd = 2,

        col = cols[g]

      )

      points(

        c,
        j,

        pch = 19,

        col = cols[g]

      )

    }

  }

  # --------------------------------
  # Legend outside plotting region
  # --------------------------------

  usr <- par("usr")

  offset <- 0.05 * (usr[2] - usr[1])

  legend(

    x = usr[2] + offset,

    y = usr[4],

    legend = paste(

      "Cluster",

      seq_len(k)

    ),

    col = cols[seq_len(k)],

    pch = 19,

    pt.cex = 1.3,

    lty = 0,

    bty = "n",

    xpd = TRUE,

    xjust = 0,

    yjust = 1

  )

  box()

  invisible(NULL)

}
