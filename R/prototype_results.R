#' Prototype Results
#'
#' Returns cluster prototypes either as a flat matrix
#' or as a list of tables containing l, c, r values
#' for each variable.
#'
#' @param object An object of class "fcmTFN".
#' @param use_var_names Logical.
#' @param var_names Optional variable names.
#' @param format Character string indicating the output format:
#'   "flat" (default) returns the original matrix representation,
#'   while "table" returns one table per cluster.
#'
#' @return
#' If format = "flat", a data.frame containing prototype values.
#'
#' If format = "table", a named list of data.frames, one for each cluster.
#'
#' @export

prototype_results <- function(
    object,
    use_var_names = FALSE,
    var_names = NULL,
    format = c("flat", "table")
) {

  format <- match.arg(format)

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'")

  }

  if (is.null(object$prototypes)) {

    stop("Prototypes not found in object.")

  }

  prototypes <- object$prototypes

  k <- length(prototypes)

  p <- nrow(prototypes[[1]])

  if (ncol(prototypes[[1]]) != 3) {

    stop("Prototypes must have 3 columns (l, c, r).")

  }

  # --------------------------------
  # Variable names
  # --------------------------------

  if (use_var_names) {

    if (is.null(var_names)) {

      stop("Provide var_names when use_var_names = TRUE")

    }

    if (length(var_names) != p) {

      stop("Length of var_names must match number of variables.")

    }

  } else {

    var_names <- paste0(
      "Var",
      seq_len(p)
    )

  }

  # --------------------------------
  # Flat format (original behavior)
  # --------------------------------

  proto_mat <- matrix(
    NA_real_,
    nrow = p * 3,
    ncol = k
  )

  for (g in seq_len(k)) {

    proto <- prototypes[[g]]

    if (!is.matrix(proto)) {

      stop("Each prototype must be a matrix.")

    }

    if (nrow(proto) != p) {

      stop("Prototype dimension mismatch.")

    }

    proto_mat[, g] <- as.vector(proto)

  }

  colnames(proto_mat) <- paste0(
    "Cluster_",
    seq_len(k)
  )

  row_names <- character(p * 3)

  idx <- 1

  for (j in seq_len(p)) {

    row_names[idx]     <- paste0(var_names[j], "_l")
    row_names[idx + 1] <- paste0(var_names[j], "_c")
    row_names[idx + 2] <- paste0(var_names[j], "_r")

    idx <- idx + 3

  }

  rownames(proto_mat) <- row_names

  # --------------------------------
  # Return flat format
  # --------------------------------

  if (format == "flat") {

    return(
      round(
        as.data.frame(proto_mat),
        digits = 3
      )
    )

  }

  # --------------------------------
  # Return table format
  # --------------------------------

  proto_tables <- vector(
    mode = "list",
    length = k
  )

  for (g in seq_len(k)) {

    proto_table <- round(
      as.data.frame(
        prototypes[[g]]
      ),
      digits = 3
    )

    colnames(proto_table) <- c(
      "l",
      "c",
      "r"
    )

    rownames(proto_table) <- var_names

    proto_tables[[g]] <- proto_table

  }

  names(proto_tables) <- paste0(
    "Cluster_",
    seq_len(k)
  )

  return(proto_tables)

}
