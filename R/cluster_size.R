#' Cluster Sizes
#'
#' Computes the size of each cluster using both
#' hard assignments and fuzzy memberships.
#'
#' Hard size is obtained by assigning each observation
#' to the cluster with the highest membership degree.
#'
#' Fuzzy size is computed as the sum of membership
#' degrees for each cluster.
#'
#' @param object An object of class "fcmTFN".
#'
#' @return A data.frame containing:
#' \describe{
#'   \item{Cluster}{Cluster label.}
#'   \item{Hard_Size}{Number of observations assigned
#'   to the cluster.}
#'   \item{Fuzzy_Size}{Sum of membership degrees.}
#' }
#'
#' @export

cluster_size <- function(object) {

  # --------------------------------
  # Validate object
  # --------------------------------

  if (!inherits(object, "fcmTFN")) {

    stop("object must be of class 'fcmTFN'.")

  }

  U <- object$U

  if (is.null(U)) {

    stop("Membership matrix not found.")

  }

  # --------------------------------
  # Hard cluster sizes
  # --------------------------------

  hard_assign <- apply(
    U,
    1,
    which.max
  )

  hard_size <- as.vector(
    table(
      factor(
        hard_assign,
        levels = seq_len(ncol(U))
      )
    )
  )

  # --------------------------------
  # Fuzzy cluster sizes
  # --------------------------------

  fuzzy_size <- colSums(U)

  # --------------------------------
  # Output
  # --------------------------------

  result <- data.frame(

    Cluster = paste0(
      "Cluster_",
      seq_len(ncol(U))
    ),

    Hard_Size = hard_size,

    Fuzzy_Size = round(
      fuzzy_size,
      3
    ),

    row.names = NULL

  )

  return(result)

}

