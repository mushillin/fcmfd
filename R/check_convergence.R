#' Check Convergence of Membership Matrix
#'
#' Evaluates convergence based on the
#' Frobenius norm between two membership
#' matrices.
#'
#' @param U_old Previous membership matrix.
#' @param U_new Updated membership matrix.
#' @param epsilon Convergence tolerance.
#'
#' @return Logical value indicating convergence.
#'
#' @keywords internal
check_convergence <- function(
    U_old,
    U_new,
    epsilon = 1e-6
) {

  # Validate dimensions

  if (!all(dim(U_old) == dim(U_new))) {
    stop("U_old and U_new must have identical dimensions.")
  }

  # Compute Frobenius norm

  diff_norm <- sqrt(
    sum((U_new - U_old)^2)
  )

  # Check convergence

  return(diff_norm < epsilon)

}
