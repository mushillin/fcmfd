#' Default Symmetric Fuzzy Dictionary
#'
#' Generates predefined symmetric triangular fuzzy number (TFN)
#' dictionaries for ordinal Likert-type scales.
#'
#' Available options:
#' A: 5-point scale
#' B: 7-point scale
#' C: 10-point scale (1–10)
#' D: 11-point scale (0–10)
#'
#' @param option Character. Dictionary option ("A", "B", "C", or "D").
#'
#' @return A list containing triangular fuzzy numbers (l, c, r).
#'
#' @keywords internal
default_symmetric_dictionary <- function(option = "A") {

  # Ensure uppercase

  option <- toupper(option)

  if (option == "A") {

    dictionary <- list(
      "1" = c(l=1, c=1, r=2),
      "2" = c(l=1, c=2, r=3),
      "3" = c(l=2, c=3, r=4),
      "4" = c(l=3, c=4, r=5),
      "5" = c(l=4, c=5, r=5)
    )

  } else if (option == "B") {

    dictionary <- list(
      "1" = c(l=1, c=1, r=2),
      "2" = c(l=1, c=2, r=3),
      "3" = c(l=2, c=3, r=4),
      "4" = c(l=3, c=4, r=5),
      "5" = c(l=4, c=5, r=6),
      "6" = c(l=5, c=6, r=7),
      "7" = c(l=6, c=7, r=7)
    )

  } else if (option == "C") {

    dictionary <- list(
      "1"  = c(l=1, c=1, r=2),
      "2"  = c(l=1, c=2, r=3),
      "3"  = c(l=2, c=3, r=4),
      "4"  = c(l=3, c=4, r=5),
      "5"  = c(l=4, c=5, r=6),
      "6"  = c(l=5, c=6, r=7),
      "7"  = c(l=6, c=7, r=8),
      "8"  = c(l=7, c=8, r=9),
      "9"  = c(l=8, c=9, r=10),
      "10" = c(l=9, c=10, r=10)
    )

  } else if (option == "D") {

    dictionary <- list(
      "0"  = c(l=0, c=0, r=1),
      "1"  = c(l=0, c=1, r=2),
      "2"  = c(l=1, c=2, r=3),
      "3"  = c(l=2, c=3, r=4),
      "4"  = c(l=3, c=4, r=5),
      "5"  = c(l=4, c=5, r=6),
      "6"  = c(l=5, c=6, r=7),
      "7"  = c(l=6, c=7, r=8),
      "8"  = c(l=7, c=8, r=9),
      "9"  = c(l=8, c=9, r=10),
      "10" = c(l=9, c=10, r=10)
    )

  } else {

    stop(
      "Invalid symmetric dictionary option. ",
      "Available options are 'A', 'B', 'C', or 'D'."
    )

  }

  invisible(dictionary)

}
