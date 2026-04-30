#' Prototype Matrix Extraction
#'
#' Returns cluster prototypes as a readable matrix
#' containing l, c, r values for each variable.
#'
#' @param object An object of class "fcmTFN".
#' @param use_var_names Logical.
#' @param var_names Optional variable names.
#'
#' @return A data.frame containing prototype values.
#'
#' @export

prototype_matrix <- function(
    object,
    use_var_names = FALSE,
    var_names = NULL
) {

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
  # Create matrix
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

    # ✔ CORRECCIÓN AQUÍ
    proto_mat[, g] <- as.vector(proto)

  }

  colnames(proto_mat) <- paste0(
    "Cluster_",
    seq_len(k)
  )

  # --------------------------------
  # Row names
  # --------------------------------

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
  # Return as data.frame
  # --------------------------------

  proto_df <- as.data.frame(proto_mat)

  return(proto_df)

}
