#' Extract Membership Matrix
#'
#' Returns the fuzzy membership matrix obtained
#' from the Fuzzy C-Means clustering process.
#'
#' @param object An object of class "fcmTFN".
#'
#' @return A matrix where rows represent observations
#' and columns represent clusters.
#'
#' @export

membership <- function(object) {

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'")

  }

  if (is.null(object$U)) {

    stop("Membership matrix U not found in object.")

  }

  U <- object$U

  colnames(U) <- paste0(
    "Cluster_",
    seq_len(ncol(U))
  )

  return(U)

}

