#' Simulated Likert Data (0–10 Scale)
#'
#' A synthetic dataset representing ordinal Likert-type responses
#' measured on a 0–10 scale.
#'
#' The dataset is designed with an underlying cluster structure
#' (low, medium, high profiles) to support clustering validation.
#'
#' @format A data.frame with 500 observations and 10 variables:
#' \describe{
#'   \item{life_satisfaction}{Overall life satisfaction}
#'   \item{happiness}{Self-reported happiness}
#'   \item{anxiety}{Anxiety level}
#'   \item{depression}{Depression level}
#'   \item{health}{Self-rated health}
#'   \item{income_satisfaction}{Income satisfaction}
#'   \item{job_satisfaction}{Job satisfaction}
#'   \item{social_relationships}{Social relationships quality}
#'   \item{trust_in_others}{Trust in others}
#'   \item{future_expectations}{Future expectations}
#' }
#'
#' @details
#' The dataset contains three latent groups representing
#' different levels of well-being.
#'
#' @source Simulated data
#'
"sim_likert_0_10"
