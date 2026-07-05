#' Membership Quality Assessment
#'
#' Evaluates the quality of fuzzy assignments
#' for each cluster using membership-based measures.
#'
#' @details
#' This function computes both cluster-specific metrics based on
#' hardened assignments (maximum membership rule) and global partition
#' criteria to evaluate how well-defined the fuzzy boundaries are.
#'
#' @param object An object of class "fcmTFN".
#'
#' @return A list containing:
#' \describe{
#'   \item{cluster_summary}{
#'   Data frame with one row per cluster containing:
#'   \itemize{
#'     \item Hard_Size: number of observations
#'     primarily assigned to the cluster.
#'     \item Avg_Membership: average membership
#'     of those observations.
#'     \item Avg_Margin: average difference between
#'     the largest and second-largest memberships,
#'     indicating the clarity of assignment.
#'   }}
#'   \item{partition_coefficient}{
#'   Global partition coefficient (PC).}
#'   \item{partition_entropy}{
#'   Global partition entropy (PE).}
#' }
#'
#' @export
membership_quality <- function(object) {

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

  n <- nrow(U)

  k <- ncol(U)

  # --------------------------------
  # Hard assignments
  # --------------------------------

  hard_cluster <- apply(
    U,
    1,
    which.max
  )

  # --------------------------------
  # Membership margin
  # --------------------------------

  margin <- apply(
    U,
    1,
    function(x) {

      s <- sort(
        x,
        decreasing = TRUE
      )

      s[1] - s[2]

    }
  )

  # --------------------------------
  # Cluster summary
  # --------------------------------

  cluster_summary <- data.frame(

    Cluster = paste0(
      "Cluster_",
      seq_len(k)
    ),

    Hard_Size = NA_integer_,

    Avg_Membership = NA_real_,

    Avg_Margin = NA_real_

  )

  for (g in seq_len(k)) {

    idx <- which(
      hard_cluster == g
    )

    cluster_summary$Hard_Size[g] <- length(idx)

    if (length(idx) > 0) {

      cluster_summary$Avg_Membership[g] <-
        round(
          mean(U[idx, g]),
          3
        )

      cluster_summary$Avg_Margin[g] <-
        round(
          mean(margin[idx]),
          3
        )

    }

  }

  # --------------------------------
  # Global measures
  # --------------------------------

  partition_coefficient <-
    round(
      sum(U^2) / n,
      3
    )

  partition_entropy <-
    round(
      -sum(U * log(U + 1e-12)) / n,
      3
    )

  # --------------------------------
  # Output
  # --------------------------------

  result <- list(

    cluster_summary =
      cluster_summary,

    partition_coefficient =
      partition_coefficient,

    partition_entropy =
      partition_entropy

  )

  return(result)

}
