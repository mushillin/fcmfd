#' Validate Input Data
#'
#' Checks that the input data object is valid
#' for fuzzy clustering analysis.
#'
#' The function verifies:
#' - Object type (data.frame or matrix)
#' - Non-zero dimensions
#' - Absence of missing values
#'
#' @param data A data.frame or matrix.
#'
#' @return Invisibly returns TRUE if validation passes.
#'
#' @keywords internal

validate_data <- function(data) {

  # Check argument presence

  if (missing(data)) {
    stop("Argument 'data' is required.")
  }

  # Check object type

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.")
  }

  # Check dimensions

  if (nrow(data) == 0) {
    stop("'data' has zero rows.")
  }

  if (ncol(data) == 0) {
    stop("'data' has zero columns.")
  }

  # Check missing values

  if (any(is.na(data))) {
    stop("'data' contains NA values.")
  }

  # Return invisible TRUE (good practice)

  invisible(TRUE)

}
