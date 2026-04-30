#' Convert Data to Triangular Fuzzy Numbers (TFN)
#'
#' Converts ordinal data into triangular fuzzy numbers
#' using the provided fuzzy dictionary.
#'
#' Each observation is transformed into a matrix
#' where rows represent variables and columns
#' correspond to (l, c, r) values.
#'
#' @param data A data.frame or matrix containing ordinal values.
#' @param dictionary A named list representing the fuzzy dictionary.
#'
#' @return A list of matrices containing TFN representations.
#'
#' @keywords internal
convert_to_tfn <- function(
    data,
    dictionary
) {

  # Get dimensions

  n <- nrow(data)
  p <- ncol(data)

  # Initialize list

  fuzzy_data <- vector(
    "list",
    n
  )

  # Loop over observations

  for (i in seq_len(n)) {

    row_tfn <- matrix(
      0,
      nrow = p,
      ncol = 3
    )

    colnames(row_tfn) <- c(
      "l",
      "c",
      "r"
    )

    # Loop over variables

    for (j in seq_len(p)) {

      value <- as.character(
        data[i, j]
      )

      # Retrieve TFN

      tfn <- dictionary[[value]]

      # Safety check

      if (is.null(tfn)) {
        stop(
          paste(
            "Value not found in dictionary:",
            value
          )
        )
      }

      row_tfn[j, ] <- tfn

    }

    fuzzy_data[[i]] <- row_tfn

  }

  invisible(fuzzy_data)

}
