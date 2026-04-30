#' Prepare Input System
#'
#' Prepares and validates all input components required
#' for fuzzy clustering analysis.
#'
#' The function performs the following steps:
#' 1. Validates raw data
#' 2. Builds fuzzy dictionary
#' 3. Validates data values against dictionary
#' 4. Converts data into triangular fuzzy numbers (TFN)
#'
#' @param data A data.frame or matrix containing ordinal values.
#' @param type Character. Dictionary type ("symmetric" or "asymmetric").
#' @param option Character. Scale option ("A", "B", "C", or "D").
#' @param dictionary Optional matrix for asymmetric dictionaries.
#'
#' @return A list containing processed system inputs.
#'
#' @keywords internal
prepare_input_system <- function(
    data,
    type = "symmetric",
    option = "A",
    dictionary = NULL
) {

  # Step 1 — Validate raw data

  validate_data(data)

  # Step 2 — Build dictionary

  dictionary_out <- build_dictionary(
    type = type,
    option = option,
    dictionary = dictionary
  )

  # Step 3 — Validate data values

  validate_data_values(
    data = data,
    dictionary = dictionary_out
  )

  # Step 4 — Convert data to TFN

  fuzzy_data <- convert_to_tfn(
    data = data,
    dictionary = dictionary_out
  )

  # Return structured input system

  invisible(list(

    raw_data = data,

    fuzzy_data = fuzzy_data,

    dictionary = dictionary_out,

    type = type,

    option = option

  ))

}
