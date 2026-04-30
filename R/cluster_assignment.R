#' Hard Cluster Assignment
#'
#' Assigns each observation to the cluster
#' with the highest membership value.
#'
#' @param object An object of class "fcmTFN".
#'
#' @return A factor indicating cluster labels.
#'
#' @export

cluster_assignment <- function(
    object
) {

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'")

  }

  if (is.null(object$U)) {

    stop("Membership matrix U not found in object.")

  }


  U <- object$U


  clusters <- apply(
    U,
    1,
    which.max
  )

  return(clusters)

}

