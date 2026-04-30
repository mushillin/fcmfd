#' Convert Matrix to Dictionary List
#'
#' Converts a matrix representation of triangular fuzzy numbers (TFN)
#' into a named list format used internally by the system.
#'
#' The input matrix must contain three columns:
#' (l, c, r), where:
#' l = left spread
#' c = modal value
#' r = right spread
#'
#' @param matrix A numeric matrix with columns (l, c, r).
#'
#' @return A named list containing triangular fuzzy numbers.
#'
#' @keywords internal
convert_matrix_to_list <- function(matrix) {

  # Validate input type

  if (!is.matrix(matrix)) {
    stop("Input must be a matrix.")
  }

  if (ncol(matrix) != 3) {
    stop("Matrix must have exactly 3 columns: (l, c, r).")
  }

  # Initialize list

  dictionary <- list()

  # Convert rows into list elements

  for (i in seq_len(nrow(matrix))) {

    key <- as.character(matrix[i, 2])

    dictionary[[key]] <- c(
      l = matrix[i, 1],
      c = matrix[i, 2],
      r = matrix[i, 3]
    )

  }

  invisible(dictionary)

}
