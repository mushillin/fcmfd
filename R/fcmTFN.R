#' Fuzzy C-Means Clustering for Triangular Fuzzy Numbers
#'
#' Performs fuzzy clustering on ordinal Likert-type data
#' represented as triangular fuzzy numbers (TFNs).
#'
#' The function automatically determines the optimal
#' number of clusters based on the Xie-Beni validity index.
#'
#' @references
#' Coppi, R., D'Urso, P., & Giordani, P. (2012).
#' Fuzzy and possibilistic clustering for fuzzy data.
#' <doi:10.1016/j.csda.2010.09.013>
#'
#' Xie, X. L., & Beni, G. (1991).
#' A validity measure for fuzzy clustering.
#' <doi:10.1109/34.85677>
#'
#' @importFrom graphics axis box lines par segments text
#' @importFrom stats runif
#'
#' @param data A data.frame or matrix containing ordinal Likert-type responses. Rows represent observations (respondents) and columns represent variables (items or dimensions). All values must be integers within the range defined by the selected scale option.
#'
#' @param type A character string specifying the type of fuzzy dictionary to use. Must be one of "symmetric" for predefined symmetric triangular fuzzy numbers, or "asymmetric" for a user-defined custom dictionary. Defaults to "symmetric".
#'
#' @param option A character string indicating the Likert scale option. Must be one of "A" (5-point scale, 1–5), "B" (7-point scale, 1–7), "C" (10-point scale, 1–10), or "D" (11-point scale, 0–10). Defaults to "B".
#'
#' @param dictionary An optional numeric matrix with 3 columns (l, c, r) representing the lower bound, modal value, and upper bound of each triangular fuzzy number. Required when type = "asymmetric"; ignored when type = "symmetric". Defaults to NULL.
#'
#' @param k_values A numeric vector specifying the candidate numbers of clusters to evaluate. The algorithm runs independently for each value of k. Defaults to 2:6.
#'
#' @param m Fuzzifier parameter (m > 1) specifying the fuzzifier parameter, which controls the degree of membership overlap between clusters. Higher values produce softer partitions. Defaults to 2.
#'
#' @param epsilon A positive numeric value specifying the convergence tolerance. The algorithm stops when the change in the objective function between iterations falls below this threshold. Defaults to 1e-06.
#'
#' @param max_iter A positive integer specifying the maximum number of iterations allowed per run. If convergence is not reached within this limit, a warning is issued. Defaults to 1000.
#'
#' @param verbose A logical value. If TRUE, progress messages are printed to the console during execution, including iteration count and convergence status for each value of k. Defaults to TRUE.
#'
#' @return An object of class
#' `"fcmTFN"` and `"fcm"`.
#'
#' @export

fcmTFN <- function(
    data,
    type = "symmetric",
    option = "B",
    dictionary = NULL,
    k_values = 2:6,
    m = 2,
    epsilon = 1e-6,
    max_iter = 1000,
    verbose = TRUE
) {

  # ----------------------------------------
  # Parameter validation
  # ----------------------------------------

  if (missing(data)) {
    stop("Argument 'data' is required.")
  }

  if (m <= 1) {
    stop("Parameter 'm' must be greater than 1.")
  }

  if (!is.numeric(k_values)) {
    stop("k_values must be numeric.")
  }

  if (any(k_values < 2)) {
    stop("All k_values must be >= 2.")
  }

  if (epsilon <= 0) {
    stop("epsilon must be positive.")
  }

  if (max_iter <= 0) {
    stop("max_iter must be positive.")
  }

  k_values <- unique(k_values)

  # ----------------------------------------
  # STEP 1 — Prepare input system
  # ----------------------------------------

  input_system <- prepare_input_system(
    data = data,
    type = type,
    option = option,
    dictionary = dictionary
  )

  fuzzy_data <- input_system$fuzzy_data

  # ----------------------------------------
  # STEP 2 — Determine optimal k
  # ----------------------------------------

  opt <- select_optimal_k(
    fuzzy_data = fuzzy_data,
    k_values = k_values,
    m = m,
    epsilon = epsilon,
    max_iter = max_iter,
    verbose = verbose
  )

  best_index <- which.min(
    opt$xb_values
  )

  final_result <- opt$results[[best_index]]

  # ----------------------------------------
  # STEP 3 — Build output object
  # ----------------------------------------

  result <- list(

    call = match.call(),

    best_k = opt$best_k,

    xb_values = opt$xb_values,

    k_values = k_values,

    U = final_result$U,

    prototypes = final_result$prototypes,

    wc = final_result$wc,

    ws = final_result$ws,

    iterations = final_result$iterations,

    raw_data = input_system$raw_data,

    fuzzy_data = input_system$fuzzy_data,

    dictionary = input_system$dictionary,

    type = input_system$type,

    option = input_system$option

  )

  class(result) <- c(
    "fcmTFN",
    "fcm"
  )

  return(result)

}
