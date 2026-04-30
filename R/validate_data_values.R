#' Validate Data Values Against Dictionary
#'
#' Checks that all values in the input data
#' are present in the fuzzy dictionary modes.
#'
#' The function verifies that every unique
#' value in the dataset matches a valid
#' dictionary key.
#'
#' @param data A data.frame or matrix containing ordinal values.
#' @param dictionary A named list representing the fuzzy dictionary.
#'
#' @return Invisibly returns TRUE if validation passes.
#'
#' @keywords internal
validate_data_values <- function(
    data,
    dictionary
) {

  # Extract unique values from data

  data_values <- unique(
    as.character(unlist(data))
  )

  # Extract dictionary keys

  dictionary_values <- names(dictionary)

  # Detect invalid values

  invalid_values <- setdiff(
    data_values,
    dictionary_values
  )

  # Raise error if invalid values exist

  if (length(invalid_values) > 0) {

    stop(
      paste(
        "Invalid values found in data:",
        paste(invalid_values, collapse = ", ")
      )
    )

  }

  invisible(TRUE)

}
