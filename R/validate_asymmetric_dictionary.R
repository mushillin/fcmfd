#' Validate Asymmetric Fuzzy Dictionary
#'
#' Validates a user-defined asymmetric triangular fuzzy number (TFN)
#' dictionary to ensure structural consistency with the selected scale.
#'
#' The function verifies:
#' - Object type (matrix)
#' - Number of columns (l, c, r)
#' - Number of rows according to selected option
#' - Valid modal values
#' - Logical ordering (l <= c <= r)
#' - Value bounds within allowed scale limits
#'
#' @param matrix A numeric matrix with columns (l, c, r).
#' @param option Character. Scale option ("A", "B", "C", or "D").
#'
#' @return Invisibly returns TRUE if validation passes.
#'
#' @keywords internal
validate_asymmetric_dictionary <- function(
    matrix,
    option
) {

  # Check object type

  if (!is.matrix(matrix)) {
    stop("Asymmetric dictionary must be provided as a matrix.")
  }

  # Check number of columns

  if (ncol(matrix) != 3) {
    stop("Matrix must have exactly 3 columns: (l, c, r).")
  }

  option <- toupper(option)

  # Define expected configuration

  if (option == "A") {

    expected_rows <- 5
    expected_modes <- 1:5
    min_val <- 1
    max_val <- 5

  } else if (option == "B") {

    expected_rows <- 7
    expected_modes <- 1:7
    min_val <- 1
    max_val <- 7

  } else if (option == "C") {

    expected_rows <- 10
    expected_modes <- 1:10
    min_val <- 1
    max_val <- 10

  } else if (option == "D") {

    expected_rows <- 11
    expected_modes <- 0:10
    min_val <- 0
    max_val <- 10

  } else {

    stop(
      "Invalid option. Available options are ",
      "'A', 'B', 'C', or 'D'."
    )

  }

  # Check number of rows

  if (nrow(matrix) != expected_rows) {
    stop(
      "Incorrect number of rows for selected option."
    )
  }

  # Extract columns

  l_vals <- matrix[,1]
  c_vals <- matrix[,2]
  r_vals <- matrix[,3]

  # Check modal values

  if (!all(c_vals == expected_modes)) {
    stop(
      "Column 'c' must match expected Likert modes."
    )
  }

  # Logical ordering checks

  if (!all(l_vals <= c_vals)) {
    stop("All values must satisfy l <= c.")
  }

  if (!all(c_vals <= r_vals)) {
    stop("All values must satisfy c <= r.")
  }

  # Boundary checks

  if (!all(l_vals >= min_val)) {
    stop("Values in 'l' are below allowed minimum.")
  }

  if (!all(r_vals <= max_val)) {
    stop("Values in 'r' exceed allowed maximum.")
  }

  invisible(TRUE)

}
