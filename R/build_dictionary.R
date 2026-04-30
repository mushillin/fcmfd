#' Build Fuzzy Dictionary
#'
#' Constructs a fuzzy dictionary based on the selected
#' scale type (symmetric or asymmetric).
#'
#' For symmetric dictionaries, predefined TFN structures
#' are generated. For asymmetric dictionaries, user-defined
#' matrices are validated and converted to list format.
#'
#' @param type Character. Dictionary type ("symmetric" or "asymmetric").
#' @param option Character. Scale option ("A", "B", "C", or "D").
#' @param dictionary Optional matrix for asymmetric dictionaries.
#'
#' @return A list representing the fuzzy dictionary.
#'
#' @keywords internal
build_dictionary <- function(
    type,
    option,
    dictionary = NULL
) {

  # Normalize type

  type <- tolower(type)

  # Symmetric dictionary

  if (type == "symmetric") {

    dictionary_out <- default_symmetric_dictionary(option)

  }

  # Asymmetric dictionary

  else if (type == "asymmetric") {

    if (is.null(dictionary)) {
      stop(
        "A dictionary matrix must be provided ",
        "for asymmetric type."
      )
    }

    validate_asymmetric_dictionary(
      matrix = dictionary,
      option = option
    )

    dictionary_out <- convert_matrix_to_list(
      matrix = dictionary
    )

  }

  # Invalid type

  else {

    stop(
      "Invalid dictionary type. ",
      "Available types are 'symmetric' ",
      "or 'asymmetric'."
    )

  }

  invisible(dictionary_out)

}
