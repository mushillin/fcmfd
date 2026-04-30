#' Simulated Likert 1–7 Survey Dataset
#'
#' A simulated Likert-type dataset with three
#' well-separated clusters. Designed for testing
#' fuzzy clustering of ordinal data.
#'
#' The dataset contains responses measured
#' on a 1–7 Likert scale across 12 variables.
#'
#' @format A data.frame with 300 rows and 12 variables:
#' \describe{
#'   \item{Q1}{Likert response (1–7)}
#'   \item{Q2}{Likert response (1–7)}
#'   \item{Q3}{Likert response (1–7)}
#'   \item{Q4}{Likert response (1–7)}
#'   \item{Q5}{Likert response (1–7)}
#'   \item{Q6}{Likert response (1–7)}
#'   \item{Q7}{Likert response (1–7)}
#'   \item{Q8}{Likert response (1–7)}
#'   \item{Q9}{Likert response (1–7)}
#'   \item{Q10}{Likert response (1–7)}
#'   \item{Q11}{Likert response (1–7)}
#'   \item{Q12}{Likert response (1–7)}
#' }
#'
#' @details
#' The dataset was generated using three
#' latent profiles centered approximately at:
#'
#' • Low agreement (\eqn{\approx} 2)
#' • Moderate agreement (\eqn{\approx} 4)
#' • High agreement (\eqn{\approx} 6)
#'
#' Each cluster contains 100 observations.
#'
#' @source Simulated data generated for package examples.
#'
#' @keywords datasets
#'
"sim_likert7"
